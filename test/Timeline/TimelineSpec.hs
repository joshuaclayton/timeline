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
            let (Right result) = parseGraphs "bar: 1,2,3\nline: 1,2,3\nbar: 5,5,5\nbar: -5,-5,-5"
            result `shouldBe` Graphs [BarGraph [1, 2, 3], LineGraph [1, 2, 3], BarGraph [5, 5, 5], BarGraph [-5, -5, -5]]

        it "parses stacked charts in normal order" $ do
            let (Right result) = parseGraphs "stacked-bar: [1,2,3],[4,5,6]\nline: 1,2,3"
            result `shouldBe` Graphs [StackedBar [[1, 2, 3], [4, 5, 6]], LineGraph [1, 2, 3]]

        it "parses stacked charts in reverse order" $ do
            let (Right result) = parseGraphs "line: 1,2,3\nstacked-bar: [1,2,3],[4,5,6]"
            result `shouldBe` Graphs [LineGraph [1, 2, 3], StackedBar [[1, 2, 3], [4, 5, 6]]]

        it "handles when time series lengths differ" $ do
            let (Left error) = parseGraphs "line: 1,2,3\nline: 1,2"
            error `shouldContain` "Not all graphs had the same length"

        it "handles when time series lengths differ for stacked bars" $ do
            let (Left error) = parseGraphs "stacked-bar: [1,2,3],[2,3,4]\nbar: 1,2"
            error `shouldContain` "Not all graphs had the same length"

        it "handles when stacked bar series lengths differ" $ do
            let (Left error) = parseGraphs "stacked-bar: [1,2,3],[1,2]"
            error `shouldContain` "Stacked bar items did not have equal lengths"

        it "handles when no points are provided" $ do
            let (Left error) = parseGraphs "line:\nbar:"
            error `shouldContain` "No points were provided"

        it "handles when no points are provided in one case but not another" $ do
            let (Left error) = parseGraphs "line:1,2\nbar:"
            error `shouldContain` "Not all graphs had the same length"

        it "supports sma with different values" $ do
            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(1)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 2, 4, 6, 8]]

            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(2)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 1, 3, 5, 7]]

            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(3)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 1, 2, 4, 6]]

            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(4)"
            result `shouldBe` Graphs [LineGraph [0,2,4,6,8], LineGraph [0, 1, 2, 3, 5]]

        it "supports sema with different values" $ do
            let (Right (Graphs v)) = parseGraphs "line: 0,2,4,6,8 +sma(2) +sema(0.5) +dema(0.2, 0.3) +sema(0.4) +sma(2) +dema(0.2, 0.1)"
            head v `shouldBe` LineGraph [0,2,4,6,8]
            length v `shouldBe` 7

        it "supports sma failure" $ do
            let (Left error) = parseGraphs "line: 1,2 +sma(0)"
            error `shouldContain` "SMA window must be greater than zero"

        it "supports sema failure" $ do
            let (Left error) = parseGraphs "line: 1,2 +sema(2)"
            error `shouldContain` "SEMA alpha value must be between 0 and 1"

            let (Left error') = parseGraphs "line: 1,2 +sema(-1)"
            error' `shouldContain` "SEMA alpha value must be between 0 and 1"

        it "supports dema failure" $ do
            let (Left error) = parseGraphs "line: 1,2 +dema(2, 0.5)"
            error `shouldContain` "DEMA alpha value must be between 0 and 1"

            let (Left error') = parseGraphs "line: 1,2 +dema(0.5, -1)"
            error' `shouldContain` "DEMA beta value must be between 0 and 1"

        it "supports multiple additionals" $ do
            let (Right result) = parseGraphs "line: 0,2,4,6,8 +sma(1) +sma(4)"
            result `shouldBe` Graphs [LineGraph [0, 2, 4, 6, 8], LineGraph [0, 2, 4, 6, 8], LineGraph [0, 1, 2, 3, 5]]