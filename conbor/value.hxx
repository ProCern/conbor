/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */
#pragma once

#include "cbor.hxx"
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <map>
#include <memory>
#include <ranges>
#include <span>
#include <string>
#include <tuple>
#include <unordered_set>
#include <variant>
#include <vector>

namespace conbor {
template <typename T, typename D>
concept DereferencesTo = requires(const T &t) {
    { *t } -> std::convertible_to<D>;
};
template <std::output_iterator<std::byte> O, DereferencesTo<const Value &> I>
void encode(O &output, const I &value);

/** Simple Lua value.
 * Needs a wrapping struct so it can self-reference.
 */
class Value {
  public:
    template <std::output_iterator<std::byte> O>
    friend void encode(O &, const Value &);

    using Type = std::variant<
      // Out of order so that default-constructed Value is Undefined.
      Undefined,

      std::int64_t,

      // owned byte string
      std::vector<std::byte>,

      // borrowed byte string
      BorrowedByteString,

      // owned string
      std::u8string,

      // borrowed string
      std::u8string_view,
      std::vector<std::unique_ptr<Value>>,
      std::map<std::unique_ptr<Value>, std::unique_ptr<Value>>,
      Tagged,
      bool,
      Null,
      double,
      Break>;

    Type value;

    template <class... Args>
    requires std::constructible_from<Type, Args...> Value(Args &&...t) :
        value(std::forward<Args>(t)...) {
    }

    inline Value(const std::nullptr_t) : Value(Null{}) {
    }

    template <std::output_iterator<std::byte> O>
    void encode(O &&output) const {
        ::conbor::encode(output, this);
    }

    // Encode this to cbor.
    inline std::vector<std::byte> encoded() const {
        std::vector<std::byte> output;
        encode(std::back_inserter(output));
        return output;
    }

    template <std::input_iterator I, std::sentinel_for<I> S>
        requires std::same_as<std::iter_value_t<I>, std::byte>
    void decode(I input, S last) {
        conbor::Type type{};
        std::uint64_t count;
        std::tie(type, count) = read_header(input, last);

        using enum conbor::Type;
        switch (type) {
            case conbor::Type::PositiveInteger: {
                value = static_cast<std::int64_t>(count);
                break;
            }

            case conbor::Type::NegativeInteger: {
                value = -static_cast<std::int64_t>(count) - 1;
                break;
            }
            default:
                // Not implemented yet
                std::terminate();
        }
    }

    template <std::input_iterator I, std::sentinel_for<I> S>
        requires std::same_as<std::iter_value_t<I>, std::byte>
    static inline Value decoded(I input, S last) {
        Value output;
        output.decode(input, last);
        return output;
    }

    template <std::ranges::input_range I>
        requires std::same_as<std::ranges::range_value_t<I>, std::byte>
    static inline Value decoded(I &&input) {
        Value output;
        output.decode(input.begin(), input.end());
        return output;
    }

    constexpr auto operator<=>(const Value &other) const noexcept = default;
};

/** Encode a pointer to a Value.
 * Needs to be like this, otherwise the templates will try to build the value
 * itself.
 */
template <std::output_iterator<std::byte> O>
void encode(O &output, const Value &value) {
    std::visit(
      [&output](const auto &arg) {
          ::conbor::encode(output, arg);
      },
      value.value);
}

/** Encode a pointer to a Value.
 * Needs to be like this, otherwise the templates will try to build the value
 * itself.
 */
template <std::output_iterator<std::byte> O, DereferencesTo<const Value &> I>
void encode(O &output, const I &value) {
    encode(output, *value);
}
} // namespace conbor
