/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */

#include "conbor/value.hxx"
#include <gtest/gtest.h>

// Demonstrate some basic assertions.
TEST(BuildValue, Equality) {
    EXPECT_EQ(
      conbor::Value(std::u8string_view(u8"Hello")),
      conbor::Value(std::u8string_view(u8"Hello")));
    EXPECT_EQ(
      conbor::Value(),
      conbor::Value(conbor::Undefined{})) << "Default constructed should be Undefined";
}

TEST(Encoding, Specials) {
    EXPECT_EQ(conbor::Value(false).encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(20)}) << "boolean false";
    EXPECT_EQ(conbor::Value(true).encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(21)}) << "boolean true";
    EXPECT_EQ(conbor::Value(conbor::Null{}).encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)}) << "null";
    EXPECT_EQ(conbor::Value(nullptr).encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)}) << "nullptr null";
    EXPECT_EQ(conbor::Value(conbor::Undefined{}).encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(23)}) << "undefined";
    EXPECT_EQ(conbor::Value().encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(23)}) << "default undefined";
    EXPECT_EQ(conbor::Value(conbor::Break{}).encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(31)}) << "break";
}

TEST(Encoding, PositiveInteger) {
    EXPECT_EQ(conbor::Value(5).encoded(), std::vector<std::byte>{std::byte(5)})
      << "tiny positive int";
    EXPECT_EQ(conbor::Value(24).encoded(), (std::vector<std::byte>{std::byte(24), std::byte(24)}))
      << "1 byte positive int";
    EXPECT_EQ(
      conbor::Value(256).encoded(),
      (std::vector<std::byte>{std::byte(25), std::byte(1), std::byte(0)}))
      << "2 byte positive int";
    EXPECT_EQ(
      conbor::Value(65536).encoded(),
      (std::vector<
        std::byte>{std::byte(26), std::byte(0), std::byte(1), std::byte(0), std::byte(0)}))
      << "4 byte positive int";
    EXPECT_EQ(
      conbor::Value(4294967296).encoded(),
      (std::vector<std::byte>{
        std::byte(27),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(1),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(0)}))
      << "8 byte positive int";
}

TEST(Encoding, NegativeInteger) {
    EXPECT_EQ(conbor::Value(-6).encoded(), std::vector<std::byte>{std::byte(1 << 5) | std::byte(5)})
      << "tiny negative int";
    EXPECT_EQ(
      conbor::Value(-25).encoded(),
      (std::vector<std::byte>{std::byte(1 << 5) | std::byte(24), std::byte(24)}))
      << "1 byte negative int";
    EXPECT_EQ(
      conbor::Value(-257).encoded(),
      (std::vector<std::byte>{std::byte(1 << 5) | std::byte(25), std::byte(1), std::byte(0)}))
      << "2 byte negative int";
    EXPECT_EQ(
      conbor::Value(-65537).encoded(),
      (std::vector<std::byte>{
        std::byte(1 << 5) | std::byte(26),
        std::byte(0),
        std::byte(1),
        std::byte(0),
        std::byte(0)}))
      << "4 byte negative int";
    EXPECT_EQ(
      conbor::Value(-4294967297).encoded(),
      (std::vector<std::byte>{
        std::byte(1 << 5) | std::byte(27),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(1),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(0)}))
      << "8 byte negative int";
}

/*TEST(Decoding, PositiveInteger) {
    EXPECT_EQ(conbor::Value(5), conbor::Value::decoded(std::vector<std::byte>{std::byte(5)}))
      << "tiny positive int";
    EXPECT_EQ(conbor::Value(24), (conbor::Value::decoded(std::vector<std::byte>{std::byte(24), std::byte(24)})))
      << "1 byte positive int";
    EXPECT_EQ(
      conbor::Value(256),
      (conbor::Value::decoded(std::vector<std::byte>{std::byte(25), std::byte(1), std::byte(0)})))
      << "2 byte positive int";
    EXPECT_EQ(
      conbor::Value(65536),
      (conbor::Value::decoded(std::vector<
        std::byte>{std::byte(26), std::byte(0), std::byte(1), std::byte(0), std::byte(0)})))
      << "4 byte positive int";
    EXPECT_EQ(
      conbor::Value(4294967296),
      (conbor::Value::decoded(std::vector<std::byte>{
        std::byte(27),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(1),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(0)})))
      << "8 byte positive int";
}*/

/*TEST(Decoding, NegativeInteger) {
    EXPECT_EQ(conbor::Value(-6), conbor::Value::decoded(std::vector<std::byte>{std::byte(1 << 5) | std::byte(5)}))
      << "tiny negative int";
    EXPECT_EQ(
      conbor::Value(-25),
      (conbor::Value::decoded(std::vector<std::byte>{std::byte(1 << 5) | std::byte(24), std::byte(24)})))
      << "1 byte negative int";
    EXPECT_EQ(
      conbor::Value(-257),
      (conbor::Value::decoded(std::vector<std::byte>{std::byte(1 << 5) | std::byte(25), std::byte(1), std::byte(0)})))
      << "2 byte negative int";
    EXPECT_EQ(
      conbor::Value(-65537),
      (conbor::Value::decoded(std::vector<std::byte>{
        std::byte(1 << 5) | std::byte(26),
        std::byte(0),
        std::byte(1),
        std::byte(0),
        std::byte(0)})))
      << "4 byte negative int";
    EXPECT_EQ(
      conbor::Value(-4294967297),
      (conbor::Value::decoded(std::vector<std::byte>{
        std::byte(1 << 5) | std::byte(27),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(1),
        std::byte(0),
        std::byte(0),
        std::byte(0),
        std::byte(0)})))
      << "8 byte negative int";
}*/
