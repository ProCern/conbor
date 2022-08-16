/* Copyright © 2022 Taylor C. Richberger
 * This code is released under the license described in the LICENSE file
 */

#include "conbor/cbor.hxx"
#include <gtest/gtest.h>

TEST(Encoding, Specials) {
    EXPECT_EQ(
            conbor::to_cbor(false),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(20)}))
      << "boolean false";
    EXPECT_EQ(
            conbor::to_cbor(true),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(21)}))
      << "boolean true";
    EXPECT_EQ(
            conbor::to_cbor(nullptr),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)}))
      << "null";
    EXPECT_EQ(
            conbor::to_cbor(std::optional<int>{}),
      std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)})
      << "optional null";

    EXPECT_EQ(
            conbor::to_cbor(std::optional<int>{5}),
      (std::vector<std::byte>{std::byte(5)}))
      << "optional set";
}

TEST(Encoding, Floats) {
    EXPECT_EQ(
            conbor::to_cbor(0.15625f),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(25), std::byte(0b00110001), std::byte(0b00000000)}))
      << "16 bit float";
    EXPECT_EQ(
            conbor::to_cbor(0.15625),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(25), std::byte(0b00110001), std::byte(0b00000000)}))
      << "16 bit float from double";
    EXPECT_EQ(
            conbor::to_cbor(1.0f / 3.0f),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(26), std::byte(0b00111110), std::byte(0b10101010), std::byte(0b10101010), std::byte(0b10101011)}))
      << "32 bit float";
    EXPECT_EQ(
            conbor::to_cbor(static_cast<double>(1.0f / 3.0f)),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(26), std::byte(0b00111110), std::byte(0b10101010), std::byte(0b10101010), std::byte(0b10101011)}))
      << "32 bit float from double";
    EXPECT_EQ(
            conbor::to_cbor(1.0 / 3.0),
      (std::vector<std::byte>{std::byte(7 << 5) | std::byte(27), std::byte(0b00111111), std::byte(0b11010101), std::byte(0b01010101), std::byte(0b01010101), std::byte(0b01010101), std::byte(0b01010101), std::byte(0b01010101), std::byte(0b01010101)}))
      << "64 bit float";
}

TEST(Encoding, PositiveInteger) {
    EXPECT_EQ(conbor::to_cbor(5), std::vector<std::byte>{std::byte(5)})
      << "tiny positive int";
    EXPECT_EQ(conbor::to_cbor(24), (std::vector<std::byte>{std::byte(24), std::byte(24)}))
      << "1 byte positive int";
    EXPECT_EQ(
      conbor::to_cbor(256),
      (std::vector<std::byte>{std::byte(25), std::byte(1), std::byte(0)}))
      << "2 byte positive int";
    EXPECT_EQ(
      conbor::to_cbor(65536),
      (std::vector<
        std::byte>{std::byte(26), std::byte(0), std::byte(1), std::byte(0), std::byte(0)}))
      << "4 byte positive int";
    EXPECT_EQ(
      conbor::to_cbor(4294967296),
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
    EXPECT_EQ(conbor::to_cbor(-6), std::vector<std::byte>{std::byte(1 << 5) | std::byte(5)})
      << "tiny negative int";
    EXPECT_EQ(
      conbor::to_cbor(-25),
      (std::vector<std::byte>{std::byte(1 << 5) | std::byte(24), std::byte(24)}))
      << "1 byte negative int";
    EXPECT_EQ(
      conbor::to_cbor(-257),
      (std::vector<std::byte>{std::byte(1 << 5) | std::byte(25), std::byte(1), std::byte(0)}))
      << "2 byte negative int";
    EXPECT_EQ(
      conbor::to_cbor(-65537),
      (std::vector<std::byte>{
        std::byte(1 << 5) | std::byte(26),
        std::byte(0),
        std::byte(1),
        std::byte(0),
        std::byte(0)}))
      << "4 byte negative int";
    EXPECT_EQ(
      conbor::to_cbor(-4294967297),
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
      (conbor::to_cbor(std::vector<std::byte>{std::byte(1), std::byte(3), std::byte(3), std::byte(7)})
         ),
      (std::vector<std::byte>{
        std::byte(2 << 5) | std::byte(4),
        std::byte(1),
        std::byte(3),
        std::byte(3),
        std::byte(7)}));
}

TEST(Encoding, String) {
    EXPECT_EQ(
      (conbor::to_cbor(std::u8string_view(u8"1337"))),
      (std::vector<std::byte>{
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7')}));

    EXPECT_EQ(
      (conbor::to_cbor(std::string_view("1337"))),
      (std::vector<std::byte>{
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7')}));
}

TEST(Encoding, Array) {
    EXPECT_EQ(
      conbor::to_cbor(std::vector<std::u8string>{
          u8"1337",
          u8"6969",
          }),

      (std::vector<std::byte>{
        // Header
        std::byte(4 << 5) | std::byte(2),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('6'),
        std::byte('9'),
        std::byte('6'),
        std::byte('9'),
      }));

    EXPECT_EQ(
      conbor::to_cbor(std::vector<std::vector<std::u8string>>{
          {u8"1337"},
          {u8"6969"},
          }),

      (std::vector<std::byte>{
        // Header
        std::byte(4 << 5) | std::byte(2),
        // Header
        std::byte(4 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
        // Header
        std::byte(4 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('6'),
        std::byte('9'),
        std::byte('6'),
        std::byte('9'),
      }));
}

TEST(Encoding, Map) {
    EXPECT_EQ(
      conbor::to_cbor(std::map<std::u8string, std::u8string>{
          {u8"1337",
          u8"6969",
          }
          }),

      (std::vector<std::byte>{
        // Header
        std::byte(5 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('6'),
        std::byte('9'),
        std::byte('6'),
        std::byte('9'),
      }));

    EXPECT_EQ(
      conbor::to_cbor(std::map<std::map<std::u8string, std::u8string>, std::map<std::u8string, std::u8string>>{
              {{{u8"1337", u8"6969"}}, {{u8"foo", u8"bar"}}},
          }),

      (std::vector<std::byte>{
        // Header
        std::byte(5 << 5) | std::byte(1),
        // Header
        std::byte(5 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('6'),
        std::byte('9'),
        std::byte('6'),
        std::byte('9'),
        // Header
        std::byte(5 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(3),
        std::byte('f'),
        std::byte('o'),
        std::byte('o'),
        // String
        std::byte(3 << 5) | std::byte(3),
        std::byte('b'),
        std::byte('a'),
        std::byte('r'),
      }));
}

TEST(Encoding, MapArrayMixedRecursive) {
    EXPECT_EQ(
      conbor::to_cbor(std::vector<std::map<std::vector<std::u8string>, std::vector<std::u8string>>>{
          // Vector Item as a map
          {
              // map item
              {
                  // key is a vector
                  {u8"1337", u8"6969"},
                  // value is a vector
                  {u8"foo", u8"bar"},
              }
          }
          }),

      (std::vector<std::byte>{
        // Array Header
        std::byte(4 << 5) | std::byte(1),
        // Map Header
        std::byte(5 << 5) | std::byte(1),
        // Array Header
        std::byte(4 << 5) | std::byte(2),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('6'),
        std::byte('9'),
        std::byte('6'),
        std::byte('9'),
        // Array Header
        std::byte(4 << 5) | std::byte(2),
        // String
        std::byte(3 << 5) | std::byte(3),
        std::byte('f'),
        std::byte('o'),
        std::byte('o'),
        // String
        std::byte(3 << 5) | std::byte(3),
        std::byte('b'),
        std::byte('a'),
        std::byte('r'),
      }));
}

namespace foo {
    template <conbor::ToCbor T>
    struct CborFollowsTag {
        static constexpr uint16_t id = 55799;
        T contained;

        CborFollowsTag(T contained) : contained(std::move(contained)) {
        }
    };

    template <conbor::ToCbor T>
    struct LeetTag {
        static constexpr uint16_t id = 1337;
        T contained;

        LeetTag(T contained) : contained(std::move(contained)) {
        }
    };

    template <std::output_iterator<std::byte> O, conbor::ToCbor T>
    O to_cbor(O output, const CborFollowsTag<T> &tag) {
        output = write_header(output, conbor::MajorType::SemanticTag, tag.id);

        using conbor::to_cbor;

        return to_cbor(output, tag.contained);
    }

    template <std::output_iterator<std::byte> O, conbor::ToCbor T>
    O to_cbor(O output, const LeetTag<T> &tag) {
        output = write_header(output, conbor::MajorType::SemanticTag, tag.id);

        using conbor::to_cbor;

        return to_cbor(output, tag.contained);
    }
}

TEST(Encoding, CustomTypes) {
    EXPECT_EQ(
      conbor::to_cbor(foo::CborFollowsTag(std::vector{foo::LeetTag(std::map<std::u8string, foo::CborFollowsTag<std::vector<std::u8string>>>{{u8"foo", foo::CborFollowsTag(std::vector<std::u8string>{u8"bar"})}})})),

      (std::vector<std::byte>{
       // Simple CBOR data follows prefix
        std::byte(0xd9), std::byte(0xd9), std::byte(0xf7),
        // Array Header
        std::byte(4 << 5) | std::byte(1),
       // Leet tag prefix
        std::byte(0xd9), std::byte(0x05), std::byte(0x39),
        // Map Header
        std::byte(5 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(3),
        std::byte('f'),
        std::byte('o'),
        std::byte('o'),
       // Simple CBOR data follows prefix
        std::byte(0xd9), std::byte(0xd9), std::byte(0xf7),
        // Array Header
        std::byte(4 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(3),
        std::byte('b'),
        std::byte('a'),
        std::byte('r'),
      })) << "Bidirectional decoding of internal and external types to be sure ADL does everything it should";
}

TEST(Decoding, Specials) {
    EXPECT_EQ(
            false,
      conbor::from_cbor<bool>(std::vector<std::byte>{std::byte(7 << 5) | std::byte(20)}))
      << "boolean false";
    EXPECT_EQ(
            true,
      conbor::from_cbor<bool>(std::vector<std::byte>{std::byte(7 << 5) | std::byte(21)}))
      << "boolean true";
    EXPECT_EQ(
            nullptr,
      conbor::from_cbor<std::nullptr_t>(std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)}))
      << "null";
    EXPECT_EQ(
            std::optional<int>{},
      conbor::from_cbor<std::optional<int>>(std::vector<std::byte>{std::byte(7 << 5) | std::byte(22)}))
      << "optional null";

    EXPECT_EQ(
            std::optional<int>{5},
      conbor::from_cbor<std::optional<int>>(std::vector<std::byte>{std::byte(5)}))
      << "optional set";
}

TEST(Decoding, PositiveInteger) {
    EXPECT_EQ(5, conbor::from_cbor<std::int64_t>(std::vector<std::byte>{std::byte(5)})) << "tiny positive int";
    EXPECT_EQ(24, conbor::from_cbor<std::int64_t>(std::vector<std::byte>{std::byte(24), std::byte(24)}))
      << "1 byte positive int";
    EXPECT_EQ(256,
      conbor::from_cbor<std::int64_t>(std::vector<std::byte>{std::byte(25), std::byte(1), std::byte(0)}))
      << "2 byte positive int";
    EXPECT_EQ(65536,
      conbor::from_cbor<std::int64_t>(std::vector<
        std::byte>{std::byte(26), std::byte(0), std::byte(1), std::byte(0), std::byte(0)}))
      << "4 byte positive int";
    EXPECT_EQ(4294967296,
      conbor::from_cbor<std::int64_t>(std::vector<std::byte>{
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

TEST(Decoding, NegativeInteger) {
    EXPECT_EQ(-6, conbor::from_cbor<std::int64_t>(std::vector<std::byte>{std::byte(1 << 5) | std::byte(5)})) << "tiny negative int";
    EXPECT_EQ(-25, conbor::from_cbor<std::int64_t>(std::vector<std::byte>{std::byte(1 << 5) | std::byte(24), std::byte(24)})) << "1 byte negative int";
    EXPECT_EQ(-257, conbor::from_cbor<std::int64_t>(std::vector<std::byte>{std::byte(1 << 5) | std::byte(25), std::byte(1), std::byte(0)})) << "2 byte negative int";
    EXPECT_EQ(-65537, conbor::from_cbor<std::int64_t>(std::vector<std::byte>{ std::byte(1 << 5) | std::byte(26), std::byte(0), std::byte(1), std::byte(0), std::byte(0)})) << "4 byte negative int";
    EXPECT_EQ(-4294967297, conbor::from_cbor<std::int64_t>(std::vector<std::byte>{ std::byte(1 << 5) | std::byte(27), std::byte(0), std::byte(0), std::byte(0), std::byte(1), std::byte(0), std::byte(0), std::byte(0), std::byte(0)})) << "8 byte negative int";
}

TEST(Encoding, OwnedByteString) {
    EXPECT_EQ((std::vector<std::byte>{std::byte(1), std::byte(3), std::byte(3), std::byte(7)}),
      conbor::from_cbor<std::vector<std::byte>>(std::vector<std::byte>{
        std::byte(2 << 5) | std::byte(4),
        std::byte(1),
        std::byte(3),
        std::byte(3),
        std::byte(7)}));
}

TEST(Encoding, OwnedString) {
    EXPECT_EQ(
      std::u8string(u8"1337"),
      conbor::from_cbor<std::u8string>(std::vector<std::byte>{
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7')}));
}

TEST(Decoding, Array) {
    EXPECT_EQ(
      (std::vector<std::u8string>{
          u8"1337",
          u8"6969",
          }),

      conbor::from_cbor<std::vector<std::u8string>>(std::vector<std::byte>{
        // Header
        std::byte(4 << 5) | std::byte(2),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('6'),
        std::byte('9'),
        std::byte('6'),
        std::byte('9'),
      }));

    EXPECT_EQ(
      (std::vector<std::vector<std::u8string>>{
          {u8"1337"},
          {u8"6969"},
          }),

      conbor::from_cbor<std::vector<std::vector<std::u8string>>>(std::vector<std::byte>{
        // Header
        std::byte(4 << 5) | std::byte(2),
        // Header
        std::byte(4 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('1'),
        std::byte('3'),
        std::byte('3'),
        std::byte('7'),
        // Header
        std::byte(4 << 5) | std::byte(1),
        // String
        std::byte(3 << 5) | std::byte(4),
        std::byte('6'),
        std::byte('9'),
        std::byte('6'),
        std::byte('9'),
      }));
}


/*TEST(BuildValue, Equality) {
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
    EXPECT_EQ(
      conbor::Value(24),
      (conbor::Value::decoded(std::vector<std::byte>{std::byte(24), std::byte(24)})))
      << "1 byte positive int";
    EXPECT_EQ(
      conbor::Value(256),
      (conbor::Value::decoded(std::vector<std::byte>{std::byte(25), std::byte(1), std::byte(0)})))
      << "2 byte positive int";
    EXPECT_EQ(
      conbor::Value(65536),
      (conbor::Value::decoded(
        std::vector<
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
    EXPECT_EQ(
      conbor::Value(-6),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(1 << 5) | std::byte(5)}))
      << "tiny negative int";
    EXPECT_EQ(
      conbor::Value(-25),
      (conbor::Value::decoded(
        std::vector<std::byte>{std::byte(1 << 5) | std::byte(24), std::byte(24)})))
      << "1 byte negative int";
    EXPECT_EQ(
      conbor::Value(-257),
      (conbor::Value::decoded(
        std::vector<std::byte>{std::byte(1 << 5) | std::byte(25), std::byte(1), std::byte(0)})))
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
    EXPECT_EQ(
      conbor::Value(),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(23)}))
      << "default undefined";
    EXPECT_EQ(
      conbor::Value(conbor::Break{}),
      conbor::Value::decoded(std::vector<std::byte>{std::byte(7 << 5) | std::byte(31)}))
      << "break";
}*/
