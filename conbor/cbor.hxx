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
#include <ranges>
#include <tuple>
#include <utility>
#include <vector>

namespace sard {
namespace cbor {
/** Encode the value into output.
 */
template <class O>
requires std::output_iterator<O, std::byte>
void encode(O output, const std::uint64_t value, const bool negative) {
    const auto type = std::byte(static_cast<unsigned>(negative) << 5);

    if (value < 24) {
        *output = (type | std::byte(value));
        ++output;
    } else if (value < 0x100) {
        *output = (type | std::byte(24));
        ++output;
        *output = std::byte(value);
        ++output;
    } else if (value < 0x10000) {
        *output = (type | std::byte(25));
        ++output;
        *output = std::byte(value >> 8);
        ++output;
        *output = std::byte(value);
        ++output;
    } else if (value < 0x1000000) {
        *output = (type | std::byte(26));
        ++output;
        *output = std::byte(value >> 24);
        ++output;
        *output = std::byte(value >> 16);
        ++output;
        *output = std::byte(value >> 8);
        ++output;
        *output = std::byte(value);
        ++output;
    } else {
        *output = (type | std::byte(27));
        ++output;
        *output = std::byte(value >> 56);
        ++output;
        *output = std::byte(value >> 48);
        ++output;
        *output = std::byte(value >> 40);
        ++output;
        *output = std::byte(value >> 32);
        ++output;
        *output = std::byte(value >> 24);
        ++output;
        *output = std::byte(value >> 16);
        ++output;
        *output = std::byte(value >> 8);
        ++output;
        *output = std::byte(value);
        ++output;
    }
}

/** Encode the byte string.
 */
template <class O, std::ranges::input_range R>
requires std::output_iterator<O, std::byte> && std::ranges::sized_range<R> &&
  std::same_as<std::ranges::range_value_t<R>, std::byte>
void encode(O output, const R &value) {
    const auto size = value.size();

    if (size < 24) {
        *output = std::byte(2 << 5) | std::byte(size);
        ++output;
    } else if (size < 0x100ul) {
        *output = std::byte(2 << 5) | std::byte(24);
        ++output;
        *output = std::byte(size);
        ++output;
    } else if (size < 0x10000ul) {
        *output = std::byte(2 << 5) | std::byte(25);
        ++output;
        *output = std::byte(size >> 8);
        ++output;
        *output = std::byte(size);
        ++output;
    } else if (size < 0x1000000ul) {
        *output = std::byte(2 << 5) | std::byte(26);
        ++output;
        *output = std::byte(size >> 24);
        ++output;
        *output = std::byte(size >> 16);
        ++output;
        *output = std::byte(size >> 8);
        ++output;
        *output = std::byte(size);
        ++output;
    } else {
        *output = std::byte(2 << 5) | std::byte(27);
        ++output;
        *output = std::byte(size >> 56);
        ++output;
        *output = std::byte(size >> 48);
        ++output;
        *output = std::byte(size >> 40);
        ++output;
        *output = std::byte(size >> 32);
        ++output;
        *output = std::byte(size >> 24);
        ++output;
        *output = std::byte(size >> 16);
        ++output;
        *output = std::byte(size >> 8);
        ++output;
        *output = std::byte(size);
        ++output;
    }
    std::ranges::copy(value, output);
}

/** Pair concept.
 */
template <typename T>
concept Pair = std::tuple_size<T>::value == 2;

/** References a mapping range.
 */
template <typename T>
concept EncodeablePairRange = requires(const T &t) {
    requires Pair<std::iter_value_t<std::ranges::iterator_t<T>>>;

    encode(std::vector<std::byte>{}.begin(), std::get<0>(t.begin()));
    encode(std::vector<std::byte>{}.begin(), std::get<1>(t.begin()));
};

/** Encode a map.
 */
template <class O, EncodeablePairRange R>
requires std::output_iterator<O, std::byte> && std::ranges::sized_range<R> &&
  std::ranges::input_range<R>
void encode(O output, const R &value) {
    const auto size = value.size();

    if (size < 24) {
        *output = std::byte(5 << 5) | std::byte(size);
        ++output;
    } else if (size < 0x100ul) {
        *output = std::byte(5 << 5) | std::byte(24);
        ++output;
        *output = std::byte(size);
        ++output;
    } else if (size < 0x10000ul) {
        *output = std::byte(5 << 5) | std::byte(25);
        ++output;
        *output = std::byte(size >> 8);
        ++output;
        *output = std::byte(size);
        ++output;
    } else if (size < 0x1000000ul) {
        *output = std::byte(5 << 5) | std::byte(26);
        ++output;
        *output = std::byte(size >> 24);
        ++output;
        *output = std::byte(size >> 16);
        ++output;
        *output = std::byte(size >> 8);
        ++output;
        *output = std::byte(size);
        ++output;
    } else {
        *output = std::byte(5 << 5) | std::byte(27);
        ++output;
        *output = std::byte(size >> 56);
        ++output;
        *output = std::byte(size >> 48);
        ++output;
        *output = std::byte(size >> 40);
        ++output;
        *output = std::byte(size >> 32);
        ++output;
        *output = std::byte(size >> 24);
        ++output;
        *output = std::byte(size >> 16);
        ++output;
        *output = std::byte(size >> 8);
        ++output;
        *output = std::byte(size);
        ++output;
    }
    for (const auto &[k, v] : value) {
        encode(output, k);
        encode(output, v);
    }
}

template <class O>
requires std::output_iterator<O, std::byte>
void encode(O output, const std::int64_t value) {
    // Only works for two's complement
    if (value == std::numeric_limits<std::int64_t>::min()) {
        encode(
          output,
          static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()) + 1,
          true);
    } else {
        encode(output, static_cast<std::uint64_t>(std::abs(value)), value < 0);
    }
}

template <class O>
requires std::output_iterator<O, std::byte>
void encode(O output, const std::uint64_t value) {
    encode(output, value, false);
}

template <class O>
requires std::output_iterator<O, std::byte>
void encode(O output, const bool value) {
    if (value) {
        *output = (std::byte(7 << 5) | std::byte(21));
    } else {
        *output = (std::byte(7 << 5) | std::byte(20));
    }
    ++output;
}

template <class O>
requires std::output_iterator<O, std::byte>
void encode(O output, const std::nullptr_t) {
    *output = (std::byte(7 << 5) | std::byte(22));
    ++output;
}

template <class O, std::floating_point P>
requires std::output_iterator<O, std::byte>
inline void encode(O output, const P value) {
    const double d = value;
    const float f = value;

    static_assert(sizeof(float) == 4, "floats must be 4 bytes");
    static_assert(sizeof(double) == 8, "doubles must be 8 bytes");
    static_assert(
      std::endian::native == std::endian::big || std::endian::native == std::endian::little,
      "mixed endian architectures can not be supported yet");

    // TODO: float16
    if (static_cast<double>(f) == d) {
        *output = (std::byte(7 << 5) | std::byte(26));
        ++output;
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
        *output = (std::byte(7 << 5) | std::byte(27));
        ++output;
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
} // namespace cbor
} // namespace sard
