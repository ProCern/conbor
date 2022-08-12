/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */
#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <bitset>
#include <cmath>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <memory>
#include <ranges>
#include <span>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace conbor {
    /** Struct to force ACL for internal types.
     */
    struct Adl {
    };

/** A type that can be encoded to cbor.
 */
template <typename T, typename O>
concept ToCborInternal = requires(const T &t, O o, Adl adl) {
    requires std::output_iterator<O, std::byte>;

    { to_cbor(o, t, adl) } -> std::same_as<O>;
};

/** A type that can be encoded to cbor.
 */
template <typename T, typename O>
concept ToCborExternal = requires(const T &t, O o) {
    requires std::output_iterator<O, std::byte>;

    { to_cbor(o, t) } -> std::same_as<O>;
};

template <typename T, typename O>
concept ToCbor = requires(const T &t, O o) {
    requires ToCborExternal<T, O> || ToCborInternal<T, O>;
};


/** Pair concept.
 */
template <typename T>
concept Pair = std::tuple_size<T>::value == 2;

/** Constrains an array range
 */
template <typename T, typename O>
concept ToCborRange = requires {
    requires std::ranges::input_range<T>;
    requires ToCbor<std::ranges::range_value_t<T>, O>;
};

/** Constrains a mapping range.
 */
template <typename T, typename O>
concept ToCborPairRange = requires {
    requires std::ranges::input_range<T>;
    requires Pair<std::ranges::range_value_t<T>>;
    requires ToCbor<typename std::tuple_element<0, std::ranges::range_value_t<T>>::type, O>;
    requires ToCbor<typename std::tuple_element<1, std::ranges::range_value_t<T>>::type, O>;
};


/** Default error type.
 */
class Error : public std::runtime_error {
  public:
    template <class... Args>
    requires std::constructible_from<std::runtime_error, Args...> Error(Args &&...t) :
        runtime_error(std::forward<Args>(t)...) {
    }
};

/** End of input error.
 */
class EndOfInput : public Error {
  public:
    template <class... Args>
    requires std::constructible_from<Error, Args...> EndOfInput(Args &&...t) :
        Error(std::forward<Args>(t)...) {
    }
};

/** Illegal SpecialFloat.
 */
class IllegalSpecialFloat : public Error {
  public:
    template <class... Args>
    requires std::constructible_from<Error, Args...> IllegalSpecialFloat(Args &&...t) :
        Error(std::forward<Args>(t)...) {
    }
};

/** Tried to extract a count when the count wasn't normal
 */
class SpecialCountError : public Error {
  public:
    template <class... Args>
    requires std::constructible_from<Error, Args...> SpecialCountError(Args &&...t) :
        Error(std::forward<Args>(t)...) {
    }
};

/** Peek at a single byte, returning an error if input == last.
 */
template <
  std::input_iterator I,
  std::sentinel_for<I> S>

  requires std::same_as<std::iter_value_t<I>, std::byte>
inline std::byte peek(I &input, S last) {
    if (input == last) {
        throw EndOfInput("Reached end of input early");
    }
    return *input;
}

/** Read a single byte, returning an error if input == last.
 */
template <
  std::input_iterator I,
  std::sentinel_for<I> S>
  requires std::same_as<std::iter_value_t<I>, std::byte>
inline std::byte read(I &input, S last) {
    const auto output = peek(input, last);
    ++input;
    return output;
}

/** The major type read from the header.
 */
enum class MajorType {
    PositiveInteger = 0,
    NegativeInteger = 1,
    ByteString = 2,
    Utf8String = 3,
    Array = 4,
    Map = 5,
    SemanticTag = 6,
    SpecialFloat = 7
};

/** The count read from the header.  If the count is 24-27, the extended count
 * field is delivered in one of the last four variants, otherwise, it is simply
 * the first variant.
 */
using Count = std::variant<std::uint8_t, std::uint8_t, std::uint16_t, std::uint32_t, std::uint64_t>;

/** Extract a full count from the variant,  throwing an error if the count
 * wouldn't be a legal count or would be ambiguous (like any value in the tiny
 * field of more than 23).  This means that getting a 31 in the tiny field as a
 * break value is illegal.
 */
inline std::uint64_t extract(const Count count) {
    if (count.index() == 0) {
        const auto tinycount = std::get<0>(count);
        if (tinycount < 24) {
            return tinycount;
        } else {
            throw SpecialCountError("Tiny count would be ambiguous");
        }
    }
}

using Header = std::tuple<MajorType, Count>;

template <
  std::input_iterator I,
  std::sentinel_for<I> S>
  requires std::same_as<std::iter_value_t<I>, std::byte> Header read_header(I &input, S last) noexcept {

    const auto byte = read(input, last);
    const auto type = static_cast<MajorType>(byte >> 5);
    const auto tinycount = static_cast<std::uint8_t>(byte & std::byte(0b00011111));

    switch (tinycount) {
    case 24:
        return Header{type, Count(std::in_place_index<1>, static_cast<uint8_t>(read(input, last)))};

    case 25: {
        uint16_t count = 0;
        for (size_t i = 0; i < sizeof(count); ++i) {
            count = (count << 8) | static_cast<decltype(count)>(read(input, last));
        }
        return Header{type, Count(std::in_place_index<2>, count)};
    }

    case 26: {
        uint32_t count = 0;
        for (size_t i = 0; i < sizeof(count); ++i) {
            count = (count << 8) | static_cast<decltype(count)>(read(input, last));
        }
        return Header{type, Count(std::in_place_index<3>, count)};
    }

    case 27: {
        uint64_t count = 0;
        for (size_t i = 0; i < sizeof(count); ++i) {
            count = (count << 8) | static_cast<decltype(count)>(read(input, last));
        }
        return Header{type, Count(std::in_place_index<4>, count)};
    }

    default:
        return Header{type, Count(std::in_place_index<0>, tinycount)};
    }
}

/** Write the header.
 */
template <std::output_iterator<std::byte> O>
O write_header(O output, const Header header) {
    const auto [type, count] = header;

    const auto type_byte = std::byte(static_cast<std::uint8_t>(type) << 5);

    switch (count.index()) {
    case 0: {
        *output = (type_byte | std::byte(std::get<0>(count)));
        ++output;
        break;
    }
    case 1: {
        *output = (type_byte | std::byte(24));
        ++output;
        *output = std::byte(std::get<1>(count));
        ++output;
        break;
    }
    case 2: {
        *output = (type_byte | std::byte(25));
        ++output;
        const auto inner_count = std::get<2>(count);
        for (size_t i = 0; i < sizeof(inner_count); ++i) {
            *output = std::byte(inner_count >> ((sizeof(inner_count) - 1 - i) * 8));
            ++output;
        }
        break;
    }
    case 3: {
        *output = (type_byte | std::byte(26));
        ++output;
        const auto inner_count = std::get<3>(count);
        for (size_t i = 0; i < sizeof(inner_count); ++i) {
            *output = std::byte(inner_count >> ((sizeof(inner_count) - 1 - i) * 8));
            ++output;
        }
        break;
    }
    case 4: {
        *output = (type_byte | std::byte(27));
        ++output;
        const auto inner_count = std::get<4>(count);
        for (size_t i = 0; i < sizeof(inner_count); ++i) {
            *output = std::byte(inner_count >> ((sizeof(inner_count) - 1 - i) * 8));
            ++output;
        }
        break;
    }

    default:
        // __builtin_unreachable() or std::unreachable()
        std::terminate();
    }

    return output;
}

/** Write the definite-length header.
 *
 * Will always throw an exception for SpecialFloat.
 *
 * Ouputs the smallest appropriate single-byte or multi-byte header.
 */
template <std::output_iterator<std::byte> O>
O write_header(O output, const MajorType type, const std::uint64_t count) {
    if (type == MajorType::SpecialFloat) {
        throw IllegalSpecialFloat("SpecialFloat may not be used with the non-Header write_header function");
    }

    if (count < 24) {
        return write_header(
          output,
          Header{type, Count(std::in_place_index<0>, static_cast<std::uint8_t>(count))});
    } else if (count < 0x100ull) {
        return write_header(
          output,
          Header{type, Count(std::in_place_index<1>, static_cast<std::uint8_t>(count))});
    } else if (count < 0x10000ull) {
        return write_header(
          output,
          Header{type, Count(std::in_place_index<2>, static_cast<std::uint16_t>(count))});
    } else if (count < 0x1000000ull) {
        return write_header(
          output,
          Header{type, Count(std::in_place_index<3>, static_cast<std::uint32_t>(count))});
    } else {
        return write_header(output, Header{type, Count(std::in_place_index<4>, count)});
    }
}

/** Encode the byte string.
 */
template <std::output_iterator<std::byte> O, std::ranges::input_range R>
requires std::ranges::sized_range<R> && std::same_as<std::ranges::range_value_t<R>, std::byte>
O to_cbor(O output, const R &value, [[maybe_unused]] Adl adl) {
    output = write_header(output, MajorType::ByteString, static_cast<uint64_t>(value.size()));

    return std::ranges::copy(value, output).out;
}

// maybe TODO: indefinite-sized byte string

/** Encode the utf8 string.
 *
 * This can use char8_t or char.  It is your responsibility to ensure
 * utf8-correctness.
 */
template <std::output_iterator<std::byte> O, std::ranges::input_range R>
requires std::ranges::sized_range<R> && (std::same_as < std::ranges::range_value_t<R>, char8_t > || std::same_as < std::ranges::range_value_t<R>, char >)
O to_cbor(O output, const R &value, [[maybe_unused]] Adl adl) {
    output = write_header(output, MajorType::Utf8String, static_cast<uint64_t>(value.size()));

    return std::ranges::copy(
      value | std::views::transform([](const char8_t c) {
          return std::byte(c);
      }),
      output).out;
}

// Signed integer
template <std::output_iterator<std::byte> O, std::signed_integral I>
requires (!std::same_as<I, bool>) && (!std::same_as<I, std::byte>)&&(!std::same_as<I, char>)&&(!std::same_as<I, char8_t>)
    O to_cbor(O output, const I value, [[maybe_unused]] Adl adl) {
    if (value < 0) {
        return write_header(output, MajorType::NegativeInteger, static_cast<std::uint64_t>(std::abs(value + 1)));
    } else {
        return write_header(output, MajorType::PositiveInteger, static_cast<std::uint64_t>(value));
    }
}

// Unsigned integer
template <std::output_iterator<std::byte> O, std::unsigned_integral I>
requires (!std::same_as<I, bool>) && (!std::same_as<I, std::byte>)&&(!std::same_as<I, char>)&&(!std::same_as<I, char8_t>)
O to_cbor(O output, const I value, [[maybe_unused]] Adl adl) {
    return write_header(output, MajorType::PositiveInteger, static_cast<std::uint64_t>(value));
}

// Boolean.
// Don't want it to automatically coerce to bool, so only literal bool types are
// valid here.
template <std::output_iterator<std::byte> O>
O to_cbor(O output, const std::same_as<bool> auto value, [[maybe_unused]] Adl adl) {
    return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<0>, value ? 21 : 20)});
}

template <std::output_iterator<std::byte> O>
O to_cbor(O output, const std::nullptr_t, [[maybe_unused]] Adl adl) {
    return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<0>, 22)});
}

template <std::output_iterator<std::byte> O>
O to_cbor(O output, const std::floating_point auto value, [[maybe_unused]] Adl adl) {
    const double d = value;
    const float f = value;

    static_assert(sizeof(float) == 4, "floats must be 4 bytes");
    static_assert(sizeof(double) == 8, "doubles must be 8 bytes");
    static_assert(
      std::endian::native == std::endian::big || std::endian::native == std::endian::little,
      "mixed endian architectures can not be supported yet");

    // TODO: float16
    if (static_cast<double>(f) == d) {
        const auto f_ptr = reinterpret_cast<const std::byte *>(&f);
        std::uint32_t bytes{};
        const auto bytes_input_ptr = reinterpret_cast<std::byte *>(&bytes);

        // As long as ints and floats have the same byte order, this will always
        // output bytes big-endian.
        for (size_t i = 0; i < sizeof(f); ++i) {
            bytes_input_ptr[i] = f_ptr[i];
        }

        return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<3>, bytes)});
    } else {
        const auto d_ptr = reinterpret_cast<const std::byte *>(&d);
        std::uint64_t bytes{};
        const auto bytes_input_ptr = reinterpret_cast<std::byte *>(&bytes);
        // As long as ints and floats have the same byte order, this will always
        // output bytes big-endian.
        for (size_t i = 0; i < sizeof(d); ++i) {
            bytes_input_ptr[i] = d_ptr[i];
        }

        return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<3>, bytes)});
    }
}

/** Encode an array.
 */
template <std::output_iterator<std::byte> O, ToCborRange<O> R>
O to_cbor(O output, const R &value, [[maybe_unused]] Adl adl) {
    if constexpr (std::ranges::sized_range<R>) {
        output = write_header(output, MajorType::Array, static_cast<std::uint64_t>(value.size()));
    } else {
        output = write_header(output, Header{MajorType::Array, Count(std::in_place_index<0>, 31)});
    }
    for (const auto &item : value) {
        if constexpr (ToCborInternal<std::remove_cv_t<std::remove_reference_t<decltype(item)>>, O>) {
            output = to_cbor(output, item, adl);
        } else {
            output = to_cbor(output, item);
        }
    }
    if constexpr (!std::ranges::sized_range<R>) {
        output = write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<0>, 31)});
    }
    return output;
}

/** Encode a sized map.
 */
template <std::output_iterator<std::byte> O, ToCborPairRange<O> R>
O to_cbor(O output, const R &value, [[maybe_unused]] Adl adl) {
    if constexpr (std::ranges::sized_range<R>) {
        output = write_header(output, MajorType::Map, static_cast<std::uint64_t>(value.size()));
    } else {
        output = write_header(output, Header{MajorType::Map, Count(std::in_place_index<0>, 31)});
    }
    for (const auto &[k, v] : value) {
        if constexpr (ToCborInternal<std::remove_cv_t<std::remove_reference_t<decltype(k)>>, O>) {
            output = to_cbor(output, k, adl);
        } else {
            output = to_cbor(output, k);
        }
        if constexpr (ToCborInternal<std::remove_cv_t<std::remove_reference_t<decltype(v)>>, O>) {
            output = to_cbor(output, v, adl);
        } else {
            output = to_cbor(output, v);
        }
    }

    if constexpr (!std::ranges::sized_range<R>) {
        output = write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<0>, 31)});
    }

    return output;
}

/** Encode an internal cbor value and automatically invoke ADL
 */
template <std::output_iterator<std::byte> O, ToCborInternal<O> T>
O to_cbor(O output, const T &value) {
    return to_cbor(output, value, Adl{});
}


/** Special to_cbor convenience function that just encodes to and outputs a vector of bytes.
 */
template <class T>
requires ToCbor<T, std::back_insert_iterator<std::vector<std::byte>>>
std::vector<std::byte> to_cbor(const T &value) {
    std::vector<std::byte> output;
    to_cbor(std::back_inserter(output), value);
    return output;
}
} // namespace conbor
