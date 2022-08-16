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
#include <iostream>
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
    /** Struct to force ADL for internal types.
     */
    struct Adl {
    };

/** A type that can be encoded to cbor.
 */
template <typename T>
concept ToCborInternal = requires(const T &t, std::back_insert_iterator<std::vector<std::byte>> o, Adl adl) {
    to_cbor(o, t, adl);
};

/** A type that can be encoded to cbor.
 */
template <typename T>
concept ToCborExternal = requires(const T &t, std::back_insert_iterator<std::vector<std::byte>> o) {
    to_cbor(o, t);
};

template <typename T>
concept ToCbor = requires(const T &t) {
    requires ToCborExternal<T> || ToCborInternal<T>;
};


/** Pair concept.
 */
template <typename T>
concept Pair = std::tuple_size<T>::value == 2;

/** Constrains an array range
 */
template <typename T>
concept ToCborRange = requires {
    requires std::ranges::input_range<T>;
    requires ToCbor<std::ranges::range_value_t<T>>;
};

/** Constrains a mapping range.
 */
template <typename T>
concept ToCborPairRange = requires {
    requires std::ranges::input_range<T>;
    requires Pair<std::ranges::range_value_t<T>>;
    requires ToCbor<typename std::tuple_element<0, std::ranges::range_value_t<T>>::type>;
    requires ToCbor<typename std::tuple_element<1, std::ranges::range_value_t<T>>::type>;
};

template <typename T>
concept InputRange = std::ranges::input_range<T> && std::same_as<std::ranges::range_value_t<T>, std::byte>;

template <typename T>
concept SignedInteger = std::signed_integral<T> && (!std::same_as<T, bool>) && (!std::same_as<T, std::byte>)&&(!std::same_as<T, char>)&&(!std::same_as<T, char8_t>);

template <typename T>
concept UnsignedInteger = std::unsigned_integral<T> && (!std::same_as<T, bool>) && (!std::same_as<T, std::byte>)&&(!std::same_as<T, char>)&&(!std::same_as<T, char8_t>);

/** A type that can be decoded from cbor.
 */
template <typename T>
concept FromCborInternal = requires(T &t, std::ranges::subrange<std::istreambuf_iterator<std::byte>> i, Adl adl) {
    {from_cbor(i, t, adl)} -> InputRange;
};

/** A type that can be encoded to cbor.
 */
template <typename T>
concept FromCborExternal = requires(T &t, std::ranges::subrange<std::istreambuf_iterator<std::byte>> i) {
    { from_cbor(i, t)} -> InputRange;
};

template <typename T>
concept FromCbor = requires(const T &t) {
    requires FromCborExternal<T> || FromCborInternal<T>;
};

template <typename T, typename I>
concept PushBackable = requires (T &t, I &&i) {
    t.push_back(std::move(i));
};

template <typename T, typename I>
concept Insertable = requires (T &t, I &&i) {
    t.insert(std::move(i));
};

template <typename T, typename O>
concept Container = PushBackable<T, O> || Insertable<T, O> || std::output_iterator<T, O>;

/** Constrains an array container.
 */
template <typename T>
concept FromCborContainer = requires {
    requires Container<T, typename T::value_type>;
    requires FromCbor<typename T::value_type>;
    requires std::default_initializable<typename T::value_type>;
};

/** Constrains a mapping container.
 */
template <typename T>
concept FromCborPairContainer = requires {
    requires Pair<typename T::value_type>;
    requires Container<T, typename T::value_type>;
    requires FromCbor<std::remove_cv_t<typename std::tuple_element<0, typename T::value_type>::type>>;
    requires FromCbor<typename std::tuple_element<1, typename T::value_type>::type>;
    requires std::default_initializable<typename std::tuple_element<0, typename T::value_type>::type>;
    requires std::default_initializable<typename std::tuple_element<1, typename T::value_type>::type>;
};


/** Push into the PushBackable.
 */
template <typename O>
inline void push_into(PushBackable<O> auto &container, O &&item) {
    container.push_back(std::move(item));
}

/** Insert into the Insertable.
 */
template <typename O>
inline void push_into(Insertable<O> auto &container, O &&item) {
    container.insert(std::move(item));
}

/** Push into the iterator
 */
template <typename O>
inline void push_into(std::output_iterator<O> auto &container, O &&item) {
    *container = std::move(item);
    ++container;
}

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

class InvalidType : public Error {
  public:
    template <class... Args>
    requires std::constructible_from<Error, Args...> InvalidType(Args &&...t) :
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

template <std::ranges::range T>
using Subrange = std::ranges::subrange<std::ranges::iterator_t<T>, std::ranges::sentinel_t<T>>;

/** Peek a single byte, returning an error if input is empty.
 */
template <InputRange I>
inline std::byte peek(I input) {
    if (std::ranges::empty(input)) {
        throw EndOfInput("Reached end of input early");
    }
    auto begin = std::ranges::begin(input);
    return *begin;
}

/** Read a single byte, returning an error if input is empty.
 */
template <InputRange I>
inline Subrange<I> read(I input, std::byte &output) {
    output = peek(input);
    auto begin = std::ranges::begin(input);
    ++begin;
    return Subrange<I>(begin, std::ranges::end(input));
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
 * field of more than 23 other than 31).
 * Returns an empty optional if it was a tiny count of 31, indicating
 * indeterminant.
 */
inline std::optional<std::uint64_t> extract(const Count count) {
    if (count.index() == 0) {
        const auto tinycount = std::get<0>(count);
        if (tinycount < 24) {
            return tinycount;
        } else if (tinycount == 31) {
            return std::nullopt;
        } else {
            throw SpecialCountError("Tiny count would be ambiguous");
        }
    } else {
        return std::visit([](const auto element) { return static_cast<std::uint64_t>(element); }, count);
    }
}

//using Header = std::tuple<MajorType, Count>;
struct Header {
    MajorType type;
    Count count;
    constexpr Header() noexcept = default;
    constexpr Header(MajorType type, Count count) noexcept : type(type), count(count) {
    }

    constexpr auto operator<=>(const Header &other) const noexcept = default;
};

/** Peek the header, without changing input.
 */
template <InputRange I>
  std::tuple<MajorType, uint8_t> peek_header(I input) noexcept {
    const std::byte byte = peek(std::move(input));
    const auto type = static_cast<MajorType>(byte >> 5);
    const auto tinycount = static_cast<std::uint8_t>(byte & std::byte(0b00011111));
    return {type, tinycount};
}

template <typename T>
inline std::array<std::byte, sizeof(T)> to_be_bytes(const T &value) {
    std::array<std::byte, sizeof(T)> output;
    const auto v_pointer = reinterpret_cast<const std::byte *>(&value);


    for (size_t i = 0; i < sizeof(value); ++i) {
        if constexpr (std::endian::native == std::endian::big) {
            output[i] = v_pointer[i];
        } else {
            output[i] = v_pointer[sizeof(value) - 1 - i];
        }
    }

    return output;
}

template <typename T>
inline T from_be_bytes(std::array<std::byte, sizeof(T)> input) {
    T value;
    const auto v_pointer = reinterpret_cast<std::byte *>(&value);

    for (size_t i = 0; i < sizeof(value); ++i) {
        if constexpr (std::endian::native == std::endian::big) {
            v_pointer[i] = input[i];
        } else {
            v_pointer[i] = input[sizeof(value) - 1 - i];
        }
    }

    return value;
}

template <InputRange I>
  Subrange<I> read_header(I input, Header &header) noexcept {
    std::byte byte{};
    auto subrange = read(std::move(input), byte);
    const auto type = static_cast<MajorType>(byte >> 5);
    const auto tinycount = static_cast<std::uint8_t>(byte & std::byte(0b00011111));

    switch (tinycount) {
    case 24: {
        subrange = read(std::move(subrange), byte);
        header = Header{type, Count(std::in_place_index<1>, static_cast<uint8_t>(byte))};
        break;
    }

    case 25: {
        std::array<std::byte, 2> count;
        for (size_t i = 0; i < sizeof(count); ++i) {
            subrange = read(std::move(subrange), byte);
            count[i] = byte;
        }
        header = Header{type, Count(std::in_place_index<2>, from_be_bytes<uint16_t>(count))};
        break;
    }

    case 26: {
        std::array<std::byte, 4> count;
        for (size_t i = 0; i < sizeof(count); ++i) {
            subrange = read(std::move(subrange), byte);
            count[i] = byte;
        }
        header = Header{type, Count(std::in_place_index<3>, from_be_bytes<uint32_t>(count))};
        break;
    }

    case 27: {
        std::array<std::byte, 8> count;
        for (size_t i = 0; i < sizeof(count); ++i) {
            subrange = read(std::move(subrange), byte);
            count[i] = byte;
        }
        header = Header{type, Count(std::in_place_index<4>, from_be_bytes<uint64_t>(count))};
        break;
    }

    default: {
        header = Header{type, Count(std::in_place_index<0>, tinycount)};
        break;
    }
    }
    return subrange;
}

/** Write the header.
 */
template <std::output_iterator<std::byte> O>
O write_header(O output, const Header header) {
    const auto [type, count] = header;

    const auto type_byte = std::byte(static_cast<std::uint8_t>(type) << 5);

    if (count.index() == 0) {
        *output = (type_byte | std::byte(std::get<0>(count)));
        ++output;
    } else {
        *output = (type_byte | std::byte(23 + count.index()));
        ++output;

        std::visit([&](const auto &count) {
            output = std::ranges::copy(to_be_bytes(count), output).out;
        }, count);
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

/** Decode the byte string.
 */
template <InputRange I, Container<std::byte> O>
Subrange<I> from_cbor(I input, O &value, [[maybe_unused]] Adl adl) {
    Header header;
    auto subrange = read_header(std::move(input), header);
    switch (header.type) {
        case MajorType::ByteString: {
            const auto count = extract(header.count).value();
            for (size_t i = 0; i < count; ++i) {
                std::byte byte{};
                subrange = read(std::move(subrange), byte);
                push_into(value, std::move(byte));
            }
            break;
        };

        default: {
            throw InvalidType("Tried to read a byte string, but didn't get one");
        }
    }

    return subrange;
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

/** Decode the UTF-8 string.
 */
template <InputRange I, typename O>
requires Container<O, char8_t> || Container<O, char>
Subrange<I> from_cbor(I input, O &value, [[maybe_unused]] Adl adl) {
    using OutputType = O::value_type;
    Header header;
    auto subrange = read_header(std::move(input), header);
    switch (header.type) {
        case MajorType::Utf8String: {
            const auto count = extract(header.count).value();
            for (size_t i = 0; i < count; ++i) {
                std::byte byte{};
                subrange = read(std::move(subrange), byte);
                const auto c = static_cast<OutputType>(byte);
                push_into(value, std::move(c));
            }
            break;
        };

        default: {
            throw InvalidType("Tried to read a string, but didn't get one");
        }
    }

    return subrange;
}

template <std::output_iterator<std::byte> O>
O to_cbor(O output, const SignedInteger auto value, [[maybe_unused]] Adl adl) {
    if (value < 0) {
        return write_header(output, MajorType::NegativeInteger, static_cast<std::uint64_t>(std::abs(value + 1)));
    } else {
        return write_header(output, MajorType::PositiveInteger, static_cast<std::uint64_t>(value));
    }
}

template <InputRange I, SignedInteger Integer>
Subrange<I> from_cbor(I input, Integer &value, [[maybe_unused]] Adl adl) {
    Header header;
    auto subrange = read_header(std::move(input), header);
    switch (header.type) {
        case MajorType::NegativeInteger: {
            value = -static_cast<Integer>(extract(header.count).value()) - 1;
            break;
        }
        case MajorType::PositiveInteger: {
            value = extract(header.count).value();
            break;
        }
        default: {
            throw InvalidType("Tried to read a signed integer, but didn't get one");
        }
    }
    return subrange;
}

// Unsigned integer
template <std::output_iterator<std::byte> O>
O to_cbor(O output, const UnsignedInteger auto value, [[maybe_unused]] Adl adl) {
    return write_header(output, MajorType::PositiveInteger, static_cast<std::uint64_t>(value));
}

template <InputRange I>
Subrange<I> from_cbor(I input, UnsignedInteger auto &value, [[maybe_unused]] Adl adl) {
    Header header;
    auto subrange = read_header(std::move(input), header);
    switch (header.type) {
        case MajorType::PositiveInteger: {
            value = extract(header.count).value();
            break;
        }
        default: {
            throw InvalidType("Tried to read an unsigned integer, but didn't get one");
        }
    }
    return subrange;
}

// Boolean.
// Don't want it to automatically coerce to bool, so only literal bool types are
// valid here.
template <std::output_iterator<std::byte> O>
O to_cbor(O output, const std::same_as<bool> auto value, [[maybe_unused]] Adl adl) {
    return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<0>, value ? 21 : 20)});
}

template <InputRange I>
Subrange<I> from_cbor(I input, std::same_as<bool> auto &value, [[maybe_unused]] Adl adl) {
    Header header;
    auto subrange = read_header(std::move(input), header);
    switch (header.type) {
        case MajorType::SpecialFloat: {
            switch (extract(header.count).value()) {
                case 20:
                    value = false;
                    break;
                case 21:
                    value = true;
                    break;
                default: {
                    throw InvalidType("Tried to read a boolean, but didn't get one");
                }
            }
            break;
        }
        default: {
            throw InvalidType("Tried to read a boolean, but didn't get one");
        }
    }
    return subrange;
}

template <std::output_iterator<std::byte> O>
O to_cbor(O output, const std::nullptr_t, [[maybe_unused]] Adl adl) {
    return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<0>, 22)});
}

template <InputRange I>
Subrange<I> from_cbor(I input, [[maybe_unused]] std::nullptr_t &value, [[maybe_unused]] Adl adl) {
    Header header;
    auto subrange = read_header(std::move(input), header);
    if (!(header.type == MajorType::SpecialFloat && extract(header.count).value() == 22)) {
        throw InvalidType("Tried to read a null pointer, but didn't get one");
    }
    return subrange;
}

/** Convert from float to float16 only if it can be done losslessly.
 */
inline std::optional<std::array<std::byte, 2>> lossless_float16(const float value) {
    switch (std::fpclassify(value)) {
        case FP_ZERO: {
            std::array<std::byte, 2> output = {std::byte(0), std::byte(0)};
            if (std::signbit(value)) {
                output[0] = std::byte(0b10000000);
            }
            return output;
        }
        case FP_INFINITE: {
            std::array<std::byte, 2> output = {std::byte(0b01111100), std::byte(0)};
            if (std::signbit(value)) {
                // Positive or negative infinity
                output[0] |= std::byte(0b10000000);
            }

            return output;
        }
        case FP_NAN: {
            return std::array<std::byte, 2>{{std::byte(0b01111100), std::byte(1)}};
        }
    }
    const auto bytes = to_be_bytes(value);
    const bool sign = (bytes[0] & std::byte(0b10000000)) != std::byte(0);
    const std::uint8_t exponent =
        static_cast<std::uint8_t>(((bytes[0] & std::byte(0b01111111)) << 1)
                | ((bytes[1] & std::byte(0b10000000)) >> 7));

    const std::int8_t normalized_exponent = static_cast<std::int16_t>(exponent) - 127;
    const std::uint32_t fraction = 
        (static_cast<std::uint32_t>(bytes[1] & std::byte(0b01111111)) << 16)
        | (static_cast<std::uint32_t>(bytes[2]) << 8)
        | static_cast<std::uint32_t>(bytes[3]);

    if (normalized_exponent >= -14 && normalized_exponent <= 15 && ((fraction & 0b00000000001111111111111) == 0)) {
        // float16 5-bit biased exponent.
        const std::uint8_t biased_exponent = normalized_exponent + 15;

        std::array<std::byte, 2> output;
        if (sign) {
            output[0] = std::byte(0b10000000);
        } else {
            output[0] = std::byte(0);
        }
        output[0] |= std::byte(biased_exponent << 2);
        // Left two significant bits of 23-bit integer in fraction
        output[0] |= std::byte(fraction >> 21);
        // Bits 21 through 13 in fraction.  cut off last 13 bits, which remain
        // unused.
        output[1] = std::byte(fraction >> 13);
        return output;
    }

    return std::nullopt;
}

inline float read_float16(std::array<std::byte, 2> input) {
    const bool sign = (input[0] & std::byte(0b10000000)) != std::byte(0);
    const std::uint8_t exponent = static_cast<std::uint8_t>((input[0] & std::byte(0b01111100)) >> 2);
    const std::uint16_t fraction =
        (static_cast<std::uint16_t>(input[0] & std::byte(0b00000011)) << 8)
        | static_cast<std::uint16_t>(input[1]);
    if (exponent == 0) {
        // zero
        if (fraction == 0) {
            if (sign) {
                return -0.0f;
            } else {
                return 0.0f;
            }
        } else {
            // Subnormal.  There probably is a better way of doing this.
            const auto adjusted_fraction = static_cast<float>(fraction) / static_cast<float>(1 << 10);
            return (sign ? -1.0f : 1.0f) * adjusted_fraction * std::pow(2.0f, -14);
        }
    } else if (exponent == 0b11111) {
        // infinity
        if (fraction == 0) {
            return (sign ? -1.0f : 1.0f) * std::numeric_limits<float>::infinity();
        } else {
            // NaN
            return std::numeric_limits<float>::quiet_NaN();
        }
    } else {
        const std::int8_t normalized_exponent = static_cast<std::int8_t>(exponent) - 15;
        const auto biased_exponent = std::byte(static_cast<std::int16_t>(normalized_exponent) + 127);

        std::array<std::byte, 4> bytes = {std::byte(0), std::byte(0), std::byte(0), std::byte(0)};

        if (sign) {
            bytes[0] = std::byte(0b10000000);
        }
        bytes[0] |= biased_exponent >> 1;

        // Left bit is right bit from exponent.
        bytes[1] = biased_exponent << 7;

        // Most significant 7 bits from 10-bit fraction
        bytes[1] |= std::byte(fraction >> 3);

        // Least significant 3 bits of 10-bit fraction
        bytes[2] = std::byte(fraction << 5);

        return from_be_bytes<float>(bytes);
    }
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

    // float16 or float32
    if (std::isnan(d) || static_cast<double>(f) == d) {
        const auto float16 = lossless_float16(f);
        if (float16) {
            return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<2>, from_be_bytes<std::uint16_t>(*float16))});
        } else {
            return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<3>, from_be_bytes<std::uint32_t>(to_be_bytes(f)))});
        }
    } else {
        return write_header(output, Header{MajorType::SpecialFloat, Count(std::in_place_index<4>, from_be_bytes<std::uint64_t>(to_be_bytes(d)))});
    }
}

template <InputRange I>
Subrange<I> from_cbor(I input, std::floating_point auto &value, [[maybe_unused]] Adl adl) {
    static_assert(sizeof(float) == 4, "floats must be 4 bytes");
    static_assert(sizeof(double) == 8, "doubles must be 8 bytes");
    static_assert(
      std::endian::native == std::endian::big || std::endian::native == std::endian::little,
      "mixed endian architectures can not be supported yet");

    Header header;
    auto subrange = read_header(std::move(input), header);
    if (header.type != MajorType::SpecialFloat) {
        throw InvalidType("Tried to read a float, but didn't get one");
    }

    switch (header.count.index()) {
        case 0:
        case 1: {
            throw InvalidType("Got a Special of the wrong type.");
        }

        case 2: {
            value = read_float16(to_be_bytes(std::get<2>(header.count)));
            break;
        }

        case 3: {
            value = from_be_bytes<float>(to_be_bytes(std::get<3>(header.count)));
            break;
        }

        case 4: {
            value = from_be_bytes<double>(to_be_bytes(std::get<4>(header.count)));
            break;
        }
    }

    return subrange;
}

/** Encode an array.
 */
template <std::output_iterator<std::byte> O, ToCborRange R>
O to_cbor(O output, const R &value, [[maybe_unused]] Adl adl) {
    if constexpr (std::ranges::sized_range<R>) {
        output = write_header(output, MajorType::Array, static_cast<std::uint64_t>(value.size()));
    } else {
        output = write_header(output, Header{MajorType::Array, Count(std::in_place_index<0>, 31)});
    }
    for (const auto &item : value) {
        if constexpr (ToCborInternal<std::remove_cv_t<std::remove_reference_t<decltype(item)>>>) {
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

/** Decode an array
 */
template <InputRange I, FromCborContainer O>
Subrange<I> from_cbor(I input, O &value, [[maybe_unused]] Adl adl) {
    using OutputType = O::value_type;
    Header header;
    auto subrange = read_header(std::move(input), header);
    switch (header.type) {
        case MajorType::Array: {
            const auto count = extract(header.count);
            if (count) {
                // sized
                for (size_t i = 0; i < *count; ++i) {
                    OutputType item;
                    if constexpr (FromCborInternal<OutputType>) {
                        subrange = from_cbor(subrange, item, adl);
                    } else {
                        subrange = from_cbor(subrange, item);
                    }

                    push_into(value, std::move(item));
                }
            } else {
                // indefinite
                while (peek_header(input) != std::tuple<MajorType, uint8_t>(MajorType::SpecialFloat, 31)) {
                    OutputType item;

                    if constexpr (FromCborInternal<OutputType>) {
                        subrange = from_cbor(subrange, item, adl);
                    } else {
                        subrange = from_cbor(subrange, item);
                    }

                    push_into(value, std::move(item));
                }
            }
            break;
        };

        default: {
            throw InvalidType("Tried to read an array, but didn't get one");
        }
    }

    return subrange;
}

/** Encode a map.
 */
template <std::output_iterator<std::byte> O, ToCborPairRange R>
O to_cbor(O output, const R &value, [[maybe_unused]] Adl adl) {
    if constexpr (std::ranges::sized_range<R>) {
        output = write_header(output, MajorType::Map, static_cast<std::uint64_t>(value.size()));
    } else {
        output = write_header(output, Header{MajorType::Map, Count(std::in_place_index<0>, 31)});
    }
    for (const auto &[k, v] : value) {
        if constexpr (ToCborInternal<std::remove_cv_t<std::remove_reference_t<decltype(k)>>>) {
            output = to_cbor(output, k, adl);
        } else {
            output = to_cbor(output, k);
        }
        if constexpr (ToCborInternal<std::remove_cv_t<std::remove_reference_t<decltype(v)>>>) {
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

/** Decode a map
 */
template <InputRange I, FromCborPairContainer O>
Subrange<I> from_cbor(I input, O &value, [[maybe_unused]] Adl adl) {
    using OutputType = O::value_type;
    using KeyType = std::remove_cv_t<typename std::tuple_element<0, OutputType>::type>;
    using ValueType = std::tuple_element<1, OutputType>::type;

    Header header;
    auto subrange = read_header(std::move(input), header);
    switch (header.type) {
        case MajorType::Map: {
            const auto count = extract(header.count);
            if (count) {
                // sized
                for (size_t i = 0; i < *count; ++i) {
                    KeyType output_key;
                    if constexpr (FromCborInternal<KeyType>) {
                        subrange = from_cbor(subrange, output_key, adl);
                    } else {
                        subrange = from_cbor(subrange, output_key);
                    }

                    KeyType output_value;
                    if constexpr (FromCborInternal<ValueType>) {
                        subrange = from_cbor(subrange, output_value, adl);
                    } else {
                        subrange = from_cbor(subrange, output_value);
                    }

                    push_into(value, OutputType{std::move(output_key), std::move(output_value)});
                }
            } else {
                // indefinite
                while (peek_header(input) != std::tuple<MajorType, uint8_t>(MajorType::SpecialFloat, 31)) {
                    KeyType output_key;
                    if constexpr (FromCborInternal<KeyType>) {
                        subrange = from_cbor(subrange, output_key, adl);
                    } else {
                        subrange = from_cbor(subrange, output_key);
                    }

                    KeyType output_value;
                    if constexpr (FromCborInternal<ValueType>) {
                        subrange = from_cbor(subrange, output_value, adl);
                    } else {
                        subrange = from_cbor(subrange, output_value);
                    }

                    push_into(value, OutputType{std::move(output_key), std::move(output_value)});
                }
            }
            break;
        };

        default: {
            throw InvalidType("Tried to read a map, but didn't get one");
        }
    }

    return subrange;
}

/** Encode an optional.
 */
template <std::output_iterator<std::byte> O, ToCbor Inner>
O to_cbor(O output, const std::optional<Inner> &value, [[maybe_unused]] Adl adl) {
    if (value) {
        if constexpr (ToCborInternal<std::remove_cv_t<std::remove_reference_t<decltype(*value)>>>) {
            return to_cbor(std::move(output), *value, adl);
        } else {
            return to_cbor(std::move(output), *value);
        }
    } else {
        return to_cbor(std::move(output), nullptr, adl);
    }
}

/** Decode an optional.
 */
template <InputRange I, typename O>
requires FromCbor<O> && std::default_initializable<O>
Subrange<I> from_cbor(I input, std::optional<O> &value, [[maybe_unused]] Adl adl) {
    if (peek_header(input) == std::tuple<MajorType, uint8_t>(MajorType::SpecialFloat, 22)) {
        // null
        value.reset();
        auto begin = std::ranges::begin(input);
        ++begin;
        return Subrange<I>(begin, std::ranges::end(input));
    } else {
        value.emplace();
        if constexpr (FromCborInternal<O>) {
            return from_cbor(std::move(input), *value, adl);
        } else {
            return from_cbor(std::move(input), *value);
        }
    }
}

/** Encode an internal cbor value and automatically invoke ADL
 */
template <std::output_iterator<std::byte> O, ToCborInternal T>
O to_cbor(O output, const T &value) {
    return to_cbor(output, value, Adl{});
}


/** Special to_cbor convenience function that just encodes to and outputs a vector of bytes.
 */
std::vector<std::byte> to_cbor(const ToCbor auto &value) {
    std::vector<std::byte> output;
    to_cbor(std::back_inserter(output), value);
    return output;
}

/** Decode an internal cbor value and automatically invoke ADL
 */
template <InputRange I, FromCborInternal O>
Subrange<I> from_cbor(I input, O &value) {
    return from_cbor(std::move(input), value, Adl{});
}

template <FromCbor O, InputRange I>
requires std::default_initializable<O>
O from_cbor(I &&range) {
    O output;
    from_cbor(std::ranges::subrange(range), output);
    return output;
}
} // namespace conbor
