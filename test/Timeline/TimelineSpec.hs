module Timeline.TimelineSpec where

import Test.Hspec
import Timeline

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "parseGraphs" $ do
        it "parses line charts" $ do
            let (Right result) = parseGraphs "line: 1,2.5"
            result `shouldBe` Graphs [LineGraph [1, 2.5]]

        it "parses bar charts" $ do
            let (Right result) = parseGraphs "bar: 1.2,2"
            result `shouldBe` Graphs [BarGraph [1.2, 2]]

        it "parses stacked bar charts" $ do
            let (Right result) = parseGraphs "stacked-bar: [1,2,3],[3,4,5]"
            result `shouldBe` Graphs [StackedBar [[1, 2, 3], [3, 4, 5]]]

        it "parses multiple charts" $ do
            let (Right result) = parseGraphs "line: 1,2,3\nstacked-bar: [1,2,3],[3,4,5]"
            result `shouldBe` Graphs [LineGraph [1, 2, 3], StackedBar [[1, 2, 3], [3, 4, 5]]]

        it "handles when time series lengths differ" $ do
            let (Left error) = parseGraphs "line: 1,2,3\nline: 1,2"
            error `shouldBe` "Failed reading: Not all graphs had the same length"

        it "handles when time series lengths differ for stacked bars" $ do
            let (Left error) = parseGraphs "stacked-bar: [1,2,3],[2,3,4]\nline: 1,2"
            error `shouldBe` "Failed reading: Not all graphs had the same length"

        it "handles when stacked bar series lengths differ" $ do
            let (Left error) = parseGraphs "stacked-bar: [1,2,3],[1,2]"
            error `shouldBe` "Failed reading: Stacked bar items did not have equal lengths"

        it "handles when no points are provided" $ do
            let (Left error) = parseGraphs "line:\nbar:"
            error `shouldBe` "Failed reading: No points were provided"

        it "handles when no points are provided in one case but not another" $ do
            let (Left error) = parseGraphs "line:1,2\nbar:"
            error `shouldBe` "Failed reading: Not all graphs had the same length"

        it "supports sma with different values" $ do
            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(1)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 2, 4, 6, 8]]

            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(2)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 1, 3, 5, 7]]

            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(3)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 1, 2, 4, 6]]

            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(4)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 1, 2, 3, 5]]
