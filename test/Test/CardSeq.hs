module Test.CardSeq (tests) where

import Jasskell.Card.Seq (CardSeq)
import Jasskell.Card.Seq qualified as CardSeq
import Test.Card ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "CardSeq"
    [ testGroup
        "push"
        [ testProperty "increments length" $
            \cs c ->
              let l = CardSeq.length cs
               in l < 9 ==>
                    CardSeq.length (CardSeq.push c cs) === l + 1,
          testProperty "is indexable" $
            \cs c ->
              let l = CardSeq.length cs
               in l < 9 ==>
                    CardSeq.index (CardSeq.push c cs) l === Just c
        ],
      testGroup
        "index"
        [ testProperty "out of bounds" $ \cs (NonNegative i) ->
            CardSeq.index cs (-1 - i) === Nothing
              .&&. CardSeq.index cs (CardSeq.length cs + i) === Nothing
        ]
    ]

instance Arbitrary CardSeq where
  arbitrary = CardSeq.fromList . take 10 <$> arbitrary
  shrink = fmap CardSeq.fromList . shrink . CardSeq.toList
