# hspec-jenkins
Hspec で JUnit っぽい XML を出力する

## .cabal の設定
test-suite の build-depends に hspec-jenkins を追加する。

## Hspec の設定

Hspec にはフォーマット方法をカスタマイズする方法が用意されている。
通常、

```haskell
main = hspec spec
```

とするところを、

```haskell
import Test.Hspec.Formatters.Jenkins (xmlFormatter)

main = do
  summary <- withFile "results.xml" WriteMode $ \h -> do
    let c = defaultConfig
          { configFormatter = xmlFormatter
          , configHandle = h
          }
    hspecWith c spec
  unless (summaryFailures summary == 0) $
    exitFailure
```

のように変更する。ここでしていることは

- 出力先を results.xml へ設定
- フォーマッタとして `xmlFormatter` を指定
- 失敗したテストが1つでもあったら非0で exit (`hspec` だと勝手にこの挙動になる)

## xmlFormatter
実装としては、 blaze-markup を使って JUnit っぽい XML を出力している。

微妙な点

- 各 testcase 毎に classname, name という2つの属性を設定できるが、今は Path を元に適当にドットで結合したものを設定している
- 実行時間の情報が欠落している
    - Hspec のフォーマッタの仕様上、これを XML に含めるのは難しい
- pending (skip) したときのメッセージが Jenkins 上には表示されない
    - これは Jenkins の問題

## 利用例
[example](example) にある。
