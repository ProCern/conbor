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
    EXPECT_EQ(conbor::Value(), conbor::Value(conbor::Undefined{}))
      << "Default constructed should be Undefined";
}

TEST(Encoding, Tagged) {
    EXPECT_EQ(
      conbor::Value(conbor::Tagged(55799, true)).encoded(),
      (std::vector<std::byte>{
        std::byte(0xd9),
        std::byte(0xd9),
        std::byte(0xf7),
        std::byte(7 << 5) | std::byte(21)}))
      << "conbor tagged true";
}

TEST(Encoding, Specials) {
    EXPECT_EQ(
      conbor::Value(false).encoded(),
      std::vector<std::byte>{std::byte(7 << 5) | std::byte(20)})
      << "boolean false";
    EXPECT_EQ(
      conbor::Value(true).encoded(),
      std::vector<std::byte>{std::byte(7 << 5) | std::byte(21)})
      << "boolean true";
    EXPECT_EQ(
      conbor::Value(conbor::Null{}).encoded(),
      std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)})
      << "null";
    EXPECT_EQ(
      conbor::Value(nullptr).encoded(),
      std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)})
      << "nullptr null";
    EXPECT_EQ(
      conbor::Value(conbor::Undefined{}).encoded(),
      std::vector<std::byte>{std::byte(7 << 5) | std::byte(23)})
      << "undefined";
    EXPECT_EQ(conbor::Value().encoded(), std::vector<std::byte>{std::byte(7 << 5) | std::byte(23)})
      << "default undefined";
    EXPECT_EQ(
      conbor::Value(conbor::Break{}).encoded(),
      std::vector<std::byte>{std::byte(7 << 5) | std::byte(31)})
      << "break";
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

TEST(Encoding, ByteString) {
    EXPECT_EQ(
      (conbor::Value(std::vector<std::byte>{std::byte(1), std::byte(3), std::byte(3), std::byte(7)})
         .encoded()),
      (std::vector<std::byte>{
        std::byte(2 << 5) | std::byte(4),
        std::byte(1),
        std::byte(3),
        std::byte(3),
        std::byte(7)}));
}

TEST(Encoding, String) {
    EXPECT_EQ(
      (conbor::Value(std::u8string_view(u8"1337")).encoded()),
      (std::vector<std::byte>{
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7')}));
}

TEST(Encoding, Array) {
    std::vector<std::unique_ptr<conbor::Value>> array;
    array.push_back(std::make_unique<conbor::Value>(
      std::vector<std::byte>{std::byte(1), std::byte(3), std::byte(3), std::byte(7)}));
    array.push_back(std::make_unique<conbor::Value>(std::u8string_view(u8"1337")));
    EXPECT_EQ(
      conbor::Value(std::move(array)).encoded(),

      (std::vector<std::byte>{
        // Header
        std::byte(4 << 5) | std::byte(2),
        // Bytestring
        std::byte(2 << 5) | std::byte(4),
        std::byte(1),
        std::byte(3),
        std::byte(3),
        std::byte(7),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
      }));
}

TEST(Encoding, Map) {
    std::map<std::unique_ptr<conbor::Value>, std::unique_ptr<conbor::Value>> map;
    map.insert(std::make_pair(
      std::make_unique<conbor::Value>(
        std::vector<std::byte>{std::byte(1), std::byte(3), std::byte(3), std::byte(7)}),
      std::make_unique<conbor::Value>(std::u8string_view(u8"1337"))));
    EXPECT_EQ(
      conbor::Value(std::move(map)).encoded(),

      (std::vector<std::byte>{
        // Header
        std::byte(5 << 5) | std::byte(1),
        // Bytestring
        std::byte(2 << 5) | std::byte(4),
        std::byte(1),
        std::byte(3),
        std::byte(3),
        std::byte(7),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
      }));
}

TEST(Decoding, PositiveInteger) {
    EXPECT_EQ(conbor::Value(5), conbor::Value::decoded(std::vector<std::byte>{std::byte(5)}))
      << "tiny positive int";
    EXPECT_EQ(conbor::Value(24), (conbor::Value::decoded(std::vector<std::byte>{std::byte(24),
std::byte(24)})))
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
}

TEST(Decoding, NegativeInteger) {
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
}

TEST(Decoding, Tagged) {
    EXPECT_EQ(
      conbor::Value(conbor::Tagged(55799, true)),
      (conbor::Value::decoded(std::vector<std::byte>{
        std::byte(0xd9),
        std::byte(0xd9),
        std::byte(0xf7),
        std::byte(7 << 5) | std::byte(21)})))
      << "conbor tagged true";
}

TEST(Decoding, Specials) {
    EXPECT_EQ(
      conbor::Value(false).encoded(),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(20)}).encoded())
      << "boolean false";
    EXPECT_EQ(
      conbor::Value(false),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(20)}))
      << "boolean false";
    EXPECT_EQ(
      conbor::Value(true),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(21)}))
      << "boolean true";
    EXPECT_EQ(
      conbor::Value(conbor::Null{}),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)}))
      << "null";
    EXPECT_EQ(
      conbor::Value(nullptr),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)}))
      << "nullptr null";
    EXPECT_EQ(
      conbor::Value(conbor::Undefined{}),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(23)}))
      << "undefined";
    EXPECT_EQ(conbor::Value(),
            conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(23)}))
      << "default undefined";
    EXPECT_EQ(
      conbor::Value(conbor::Break{}),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(31)}))
      << "break";
}
