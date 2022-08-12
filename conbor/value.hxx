/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */
#pragma once

#include "cbor.hxx"
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <tuple>
#include <unordered_set>
#include <variant>
#include <vector>

namespace conbor {
class Value;

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

std::partial_ordering operator<=>(
  const std::unique_ptr<Value> &,
  const std::unique_ptr<Value> &) noexcept;
bool operator==(const std::unique_ptr<Value> &, const std::unique_ptr<Value> &) noexcept;

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
      // map
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

    // Decode, which either borrows or takes ownership.
    template <std::input_iterator I, std::sentinel_for<I> S>
    requires std::same_as<std::iter_value_t<I>, std::byte>
    void decode(I &input, S last) {
        conbor::Type type{};
        std::uint64_t count;
        std::tie(type, count) = read_header(input, last);

        using enum conbor::Type;
        switch (type) {
        case PositiveInteger: {
            value = static_cast<std::int64_t>(count);
            break;
        }

        case NegativeInteger: {
            value = -static_cast<std::int64_t>(count) - 1;
            break;
        }

        case SemanticTag: {
            value = Tagged(count, Value::decoded(input, last));
            break;
        }

        case SpecialFloat: {
            switch (count) {
            case 20:
                value = false;
                break;
            case 21:
                value = true;
                break;
            case 22:
                value = Null{};
                break;
            case 23:
                value = Undefined{};
                break;
                // TODO: floats
            case 31:
                value = Break{};
                break;
            default:
                throw Error("Illegal Special count number");
            }
            break;
        }

        default:
            // Not implemented yet
            std::terminate();
        }
    }

    template <std::input_iterator I, std::sentinel_for<I> S>
    requires std::same_as<std::iter_value_t<I>, std::byte>
    static inline Value decoded(I &input, S last) {
        Value output;
        output.decode(input, last);
        return output;
    }

    template <std::ranges::input_range I>
    requires std::same_as<std::ranges::range_value_t<I>, std::byte>
    static inline Value decoded(I &&input) {
        Value output;
        auto begin = std::ranges::begin(input);
        auto end = std::ranges::end(input);
        output.decode(begin, end);
        return output;
    }

    constexpr bool undefined() const noexcept {
        return std::holds_alternative<Undefined>(value);
    }

    constexpr std::optional<std::int64_t> integer() const noexcept {
        if (std::holds_alternative<std::int64_t>(value)) {
            return std::get<std::int64_t>(value);
        } else {
            return std::nullopt;
        }
    }

    constexpr std::optional<std::span<const std::byte>> byte_string() const noexcept {
        if (std::holds_alternative<std::vector<std::byte>>(value)) {
            return std::span<const std::byte>(std::get<std::vector<std::byte>>(value));
        } else if (std::holds_alternative<BorrowedByteString>(value)) {
            return std::get<BorrowedByteString>(value).value;
        } else {
            return std::nullopt;
        }
    }

    constexpr std::optional<std::u8string_view> u8string() const noexcept {
        if (std::holds_alternative<std::u8string_view>(value)) {
            return std::get<std::u8string_view>(value);
        } else if (std::holds_alternative<std::u8string>(value)) {
            return std::u8string_view(std::get<std::u8string>(value));
        } else {
            return std::nullopt;
        }
    }

    constexpr auto operator<=>(const Value &other) const noexcept = default;
    constexpr bool operator==(const Value &other) const noexcept = default;
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

/** Comparing unique pointers of values should actually compare the contents.
 */
std::partial_ordering operator<=>(
  const std::unique_ptr<Value> &first,
  const std::unique_ptr<Value> &second) noexcept {
    if (first && second) {
        return (*first) <=> (*second);
    } else {
        return first.get() <=> second.get();
    }
}

/** Comparing unique pointers of values should actually compare the contents.
 */
bool operator==(
  const std::unique_ptr<Value> &first,
  const std::unique_ptr<Value> &second) noexcept {
    if (first && second) {
        return (*first) == (*second);
    } else {
        return first.get() == second.get();
    }
}
} // namespace conbor
