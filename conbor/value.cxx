/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */
#include "value.hxx"

#include <stdexcept>

namespace conbor {
Value::Value(Type value) noexcept : value(std::move(value)) {
}
} // namespace conbor
