/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */
#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <span>
#include <string>
#include <tuple>
#include <unordered_set>
#include <variant>
#include <vector>

namespace conbor {
// Why isn't std::span ordered already?  std::vector is.  Weird.
constexpr std::strong_ordering operator<=>(
  const std::span<const std::byte> &a,
  const std::span<const std::byte> &b) {
    return std::basic_string_view<std::byte>(a.data(), a.size()) <=>
      std::basic_string_view<std::byte>(b.data(), b.size());
}

/** Simple Lua value.
 * Needs a wrapping struct so it can self-reference.
 */
class Value {
  public:
    struct Tagged {
        std::uint64_t tag{};
        std::unique_ptr<Value> item;

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

    using Type = std::variant<
      std::int64_t,

      // owned byte string
      std::vector<std::byte>,

      // borrowed byte string
      std::span<const std::byte>,

      // owned string
      std::u8string,

      // borrowed string
      std::u8string_view,
      std::vector<std::unique_ptr<Value>>,
      std::map<std::unique_ptr<Value>, std::unique_ptr<Value>>,
      bool,
      Null,
      Undefined,
      float,
      double,
      Break>;

  private:
    Type value;

  public:
    Value() noexcept = default;
    Value(Type value) noexcept;

    // Encode this to cbor.
    std::vector<std::byte> encode_cbor();

    // Decode to this from cbor
    static Value decode_cbor(std::span<const std::byte> data);

    constexpr auto operator<=>(const Value &other) const noexcept = default;
};
} // namespace conbor
