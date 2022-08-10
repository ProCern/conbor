/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */

#include <gtest/gtest.h>

// Demonstrate some basic assertions.
TEST(BuildValueTest, ValueEquality) {
    EXPECT_EQ(7 * 6, 42);
}
