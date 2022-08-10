/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */
#pragma once

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
#include "cbor.hxx"


namespace conbor {
    template <std::output_iterator<std::byte> O>
        void encode(O &output, const Value &value);

/** Simple Lua value.
 * Needs a wrapping struct so it can self-reference.
 */
class Value {
template <std::output_iterator<std::byte> O>
    friend void ::conbor::encode(O &, const Value &);
  public:
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

  private:
    Type value;

  public:
    template <class... Args>
    requires std::constructible_from<Type, Args...> Value(Args &&...t) :
        value(std::forward<Args>(t)...) {
    }

    inline Value(const std::nullptr_t) : Value(Null{}) {
    }

    template <std::output_iterator<std::byte> O>
    void encode(O &&output) const {
        ::conbor::encode(output, *this);
    }

    // Encode this to cbor.
    inline std::vector<std::byte> encoded() const {
        std::vector<std::byte> output;
        encode(std::back_inserter(output));
        return output;
    }

    /*template <std::input_iterator I, std::sentinel_for<I> S>
        requires std::same_as<std::iter_value_t<I>, std::byte>
    void decode(I input, S last) const {
        std::visit(
          [output](auto &&arg) {
              using T = std::decay_t<decltype(arg)>;
              if constexpr (std::is_same_v<T, std::int64_t>) {
                  ::conbor::encode(output, arg);
              } else {
                  std::terminate();
              }
          },
          value);
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
    }*/

    constexpr auto operator<=>(const Value &other) const noexcept = default;
};

/** Encode the Value.
 */
template <std::output_iterator<std::byte> O>
void encode(O &output, const Value &value) {
    std::visit(
            [&](auto &&arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, std::int64_t>
                    || std::is_same_v<T, bool>
                    || std::is_same_v<T, Null>
                    || std::is_same_v<T, Undefined>
                    || std::is_same_v<T, Break>

                    ) {
            ::conbor::encode(output, arg);
            } else {
            std::terminate();
            }
            },
        value.value);
}

template <typename T, typename D>
concept DereferencesTo = requires (const T &t) {
    {*t} -> std::convertible_to<D>;
};

/** Encode a pointer to a Value.
 */
template <std::output_iterator<std::byte> O, DereferencesTo<const Value &> I>
void encode(O &output, const I &value) {
    const Value & dereferenced = *value;
    encode(output, dereferenced);
}
} // namespace conbor
