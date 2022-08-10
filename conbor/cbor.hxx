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
#include <vector>

namespace conbor {
class Value;
    std::partial_ordering operator<=>(const std::unique_ptr<Value> &, const std::unique_ptr<Value> &) noexcept;
    bool operator==(const std::unique_ptr<Value> &, const std::unique_ptr<Value> &) noexcept;

class Error : public std::runtime_error {
  public:
    template <class... Args>
    requires std::constructible_from<std::runtime_error, Args...> Error(Args &&...t) :
        runtime_error(std::forward<Args>(t)...) {
    }
};


struct BorrowedByteString {
    std::span<const std::byte> value;

    // Why isn't std::span ordered already?  std::vector is.  Weird.
    constexpr std::strong_ordering operator<=>(const BorrowedByteString &other) const noexcept {
        return std::basic_string_view<std::byte>(value.data(), value.size()) <=>
          std::basic_string_view<std::byte>(other.value.data(), other.value.size());
    }

    template <class... Args>
    requires std::constructible_from<std::span<const std::byte>, Args...>
    constexpr BorrowedByteString(Args &&...args) : value(std::forward<Args>(args)...) {
    }

    constexpr bool operator==(const BorrowedByteString &other) const noexcept {
        return std::basic_string_view<std::byte>(value.data(), value.size()) ==
          std::basic_string_view<std::byte>(other.value.data(), other.value.size());
    }
};

struct Tagged {
    std::uint64_t tag{};
    std::unique_ptr<Value> item;

    constexpr Tagged() noexcept = default;

    inline Tagged(const std::uint64_t tag, std::unique_ptr<Value> item) :
        tag(tag),
        item(std::move(item)) {
    }

    template <class... Args>
    requires std::constructible_from<Value, Args...>
    inline Tagged(const std::uint64_t tag, Args &&...args) :
        tag(tag),
        item(std::make_unique<Value>(std::forward<Args>(args)...)) {
    }


    auto operator<=>(const Tagged &other) const noexcept = default;
};

struct Undefined {
    auto operator<=>(const Undefined &other) const noexcept = default;
};

struct Null {
    auto operator<=>(const Null &other) const noexcept = default;
};

struct Break {
    auto operator<=>(const Break &other) const noexcept = default;
};

/** Read a single byte, throwing an error if input == last
 */
template <std::input_iterator I, std::sentinel_for<I> S>
requires std::same_as<std::iter_value_t<I>, std::byte>
inline std::byte read(I &input, S last) {
    if (input == last) {
        throw Error("Reached end of input early");
    }
    const std::byte output = *input;
    ++input;
    return output;
}

enum class Type {
    PositiveInteger = 0,
    NegativeInteger = 1,
    ByteString = 2,
    Utf8String = 3,
    Array = 4,
    Map = 5,
    SemanticTag = 6,
    SpecialFloat = 7
};

    template <std::input_iterator I, std::sentinel_for<I> S>
        requires std::same_as<std::iter_value_t<I>, std::byte>
std::tuple<Type, std::uint64_t> read_header(I &input, S last) {
    const auto first_byte = read(input, last);
    const auto type = static_cast<Type>(first_byte >> 5);
    auto count = static_cast<std::uint64_t>(first_byte & std::byte(0b00011111));

    // Specialfloat just gets the count it read.  Additional processing is
    // special.
    if (type != Type::SpecialFloat) {
        if (count == 24) {
            // 8-bit count
            count = static_cast<std::uint64_t>(read(input, last));
        } else if (count == 25) {
            // 16-bit count
            count = (static_cast<std::uint64_t>(read(input, last)) << 8)
                | static_cast<std::uint64_t>(read(input, last));
        } else if (count == 26) {
            // 32-bit count
            count = (static_cast<std::uint64_t>(read(input, last)) << 24)
                | (static_cast<std::uint64_t>(read(input, last)) << 16)
                | (static_cast<std::uint64_t>(read(input, last)) << 8)
                | static_cast<std::uint64_t>(read(input, last));
        } else if (count == 27) {
            // 64-bit count
            count =
                (static_cast<std::uint64_t>(read(input, last)) << 56)
                | (static_cast<std::uint64_t>(read(input, last)) << 48)
                | (static_cast<std::uint64_t>(read(input, last)) << 40)
                | (static_cast<std::uint64_t>(read(input, last)) << 32)
                | (static_cast<std::uint64_t>(read(input, last)) << 24)
                | (static_cast<std::uint64_t>(read(input, last)) << 16)
                | (static_cast<std::uint64_t>(read(input, last)) << 8)
                | static_cast<std::uint64_t>(read(input, last));
        }
    }

    return {type, count};
}

/** Encode the value into output.
 *
 * Value is expected to already be modified if negative (cbor negative values
 * are shifted so negative 0 is impossible).
 */
template <std::output_iterator<std::byte> O>
void encode(O &output, const Type type, const std::uint64_t count) {
    const auto type_byte = std::byte(static_cast<std::uint8_t>(type) << 5);

    // SpecialFloat always just encodes the count you give it directly.  Any
    // additional special stuff you have to do yourself.
    if (count < 24 || type == Type::SpecialFloat) {
        *output = (type_byte | std::byte(count));
        ++output;
    } else if (count < 0x100ull) {
        *output = (type_byte | std::byte(24));
        ++output;
        *output = std::byte(count);
        ++output;
    } else if (count < 0x10000ull) {
        *output = (type_byte | std::byte(25));
        ++output;
        *output = std::byte(count >> 8);
        ++output;
        *output = std::byte(count);
        ++output;
    } else if (count < 0x1000000ull) {
        *output = (type_byte | std::byte(26));
        ++output;
        *output = std::byte(count >> 24);
        ++output;
        *output = std::byte(count >> 16);
        ++output;
        *output = std::byte(count >> 8);
        ++output;
        *output = std::byte(count);
        ++output;
    } else {
        *output = (type_byte | std::byte(27));
        ++output;
        *output = std::byte(count >> 56);
        ++output;
        *output = std::byte(count >> 48);
        ++output;
        *output = std::byte(count >> 40);
        ++output;
        *output = std::byte(count >> 32);
        ++output;
        *output = std::byte(count >> 24);
        ++output;
        *output = std::byte(count >> 16);
        ++output;
        *output = std::byte(count >> 8);
        ++output;
        *output = std::byte(count);
        ++output;
    }
}

/** Encode the byte string.
 */
template <std::output_iterator<std::byte> O, std::ranges::input_range R>
requires std::ranges::sized_range<R> && std::same_as<std::ranges::range_value_t<R>, std::byte>
void encode(O &output, const R &value) {
    encode(output, Type::ByteString, static_cast<uint64_t>(value.size()));

    std::ranges::copy(value, output);
}

/** Encode the borrowed byte string.
 */
template <std::output_iterator<std::byte> O>
void encode(O &output, const BorrowedByteString &value) {
    encode(output, value.value);
}

/** Encode the utf8 string.
 */
template <std::output_iterator<std::byte> O, std::ranges::input_range R>
requires std::ranges::sized_range<R> && std::same_as < std::ranges::range_value_t<R>,
char8_t > void encode(O &output, const R &value) {
    encode(output, Type::Utf8String, static_cast<uint64_t>(value.size()));

    std::ranges::copy(
      value | std::views::transform([](const char8_t c) {
          return std::byte(c);
      }),
      output);
}

/** Pair concept.
 */
template <typename T>
concept Pair = std::tuple_size<T>::value == 2;

/** References an array range
 */
template <typename T>
concept EncodeableRange = requires(const T &t, std::byte *o) {
    requires std::ranges::input_range<T>;
    encode(o, *std::ranges::begin(t));
};

/** References a mapping range.
 */
template <typename T>
concept EncodeablePairRange = requires(const T &t, std::byte *o) {
    requires std::ranges::input_range<T>;
    requires Pair<std::iter_value_t<std::ranges::iterator_t<T>>>;

    encode(o, std::get<0>(*std::ranges::begin(t)));
    encode(o, std::get<1>(*std::ranges::begin(t)));
};

/** Encode a sized array.
 */
template <std::output_iterator<std::byte> O, EncodeableRange R>
requires std::ranges::sized_range<R>
void encode(O &output, const R &value) {
    encode(output, Type::Array, static_cast<uint64_t>(value.size()));
    for (const auto &item : value) {
        encode(output, item);
    }
}

/** Encode a sized map.
 */
template <std::output_iterator<std::byte> O, EncodeablePairRange R>
requires std::ranges::sized_range<R>
void encode(O &output, const R &value) {
    encode(output, Type::Map, static_cast<uint64_t>(value.size()));
    for (const auto &[k, v] : value) {
        encode(output, k);
        encode(output, v);
    }
}

template <std::output_iterator<std::byte> O, std::signed_integral I>
requires(!std::same_as<I, bool>) &&
  (!std::same_as<I, std::byte>)&&(!std::same_as<I, char>)&&(!std::same_as<I, signed char>)&&(
    !std::same_as<I, unsigned char>)void encode(O &output, const I value) {
    if (value < 0) {
        encode(output, Type::NegativeInteger, static_cast<std::uint64_t>(std::abs(value + 1)));
    } else {
        encode(output, Type::PositiveInteger, static_cast<std::uint64_t>(value));
    }
}

template <std::output_iterator<std::byte> O, std::unsigned_integral I>
requires(!std::same_as<I, bool>) &&
  (!std::same_as<I, std::byte>)&&(!std::same_as<I, char>)&&(!std::same_as<I, signed char>)&&(
    !std::same_as<I, unsigned char>)void encode(O &output, const I value) {
    encode(output, Type::PositiveInteger, static_cast<std::uint64_t>(value));
}

template <std::output_iterator<std::byte> O>
void encode(O &output, const bool value) {
    if (value) {
        encode(output, Type::SpecialFloat, 21);
    } else {
        encode(output, Type::SpecialFloat, 20);
    }
}

template <std::output_iterator<std::byte> O>
void encode(O &output, const Tagged &tagged) {
    encode(output, Type::SemanticTag, tagged.tag);
    encode(output, tagged.item);
}

template <std::output_iterator<std::byte> O>
void encode(O &output, const Null) {
    encode(output, Type::SpecialFloat, 22);
}

template <std::output_iterator<std::byte> O>
void encode(O &output, const Undefined) {
    encode(output, Type::SpecialFloat, 23);
}

template <std::output_iterator<std::byte> O>
void encode(O &output, const Break) {
    encode(output, Type::SpecialFloat, 31);
}

template <std::output_iterator<std::byte> O, std::floating_point P>
inline void encode(O &output, const P value) {
    const double d = value;
    const float f = value;

    static_assert(sizeof(float) == 4, "floats must be 4 bytes");
    static_assert(sizeof(double) == 8, "doubles must be 8 bytes");
    static_assert(
      std::endian::native == std::endian::big || std::endian::native == std::endian::little,
      "mixed endian architectures can not be supported yet");

    // TODO: float16
    if (static_cast<double>(f) == d) {
        encode(output, Type::SpecialFloat, 26);

        const auto f_ptr = reinterpret_cast<const std::byte *>(&f);
        std::uint32_t bytes{};
        const auto bytes_input_ptr = reinterpret_cast<std::byte *>(&bytes);
        // As long as ints and floats have the same byte order, this will always
        // output bytes big-endian.
        for (size_t i = 0; i < sizeof(f); ++i) {
            bytes_input_ptr[i] = f_ptr[i];
        }
        for (size_t i = 0; i < sizeof(bytes); ++i) {
            *output = static_cast<std::byte>(bytes >> ((sizeof(bytes) - 1 - i) * 8));
            ++output;
        }
    } else {
        encode(output, Type::SpecialFloat, 27);

        const auto d_ptr = reinterpret_cast<const std::byte *>(&d);
        std::uint64_t bytes{};
        const auto bytes_input_ptr = reinterpret_cast<std::byte *>(&bytes);
        // As long as ints and floats have the same byte order, this will always
        // output bytes big-endian.
        for (size_t i = 0; i < sizeof(d); ++i) {
            bytes_input_ptr[i] = d_ptr[i];
        }
        for (size_t i = 0; i < sizeof(bytes); ++i) {
            *output = static_cast<std::byte>(bytes >> ((sizeof(bytes) - 1 - i) * 8));
            ++output;
        }
    }
}
} // namespace conbor
