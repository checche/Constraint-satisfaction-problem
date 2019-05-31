# Scalaによる制約プログラミングの実装  

## 環境  
Scala - 2.12.8  
Scala Parser Combinators - 1.1.2

## ディレクトリ構造  
重要なところのみ記載
```
├── CspFiles - CSPの問題が記述されている
└── src
    └── main
        └── scala
            ├── Application.scala - CSPを実際に計算する
            ├── CSP.scala - CSPに必要な要素の実装
            ├── CspSolver.scala - CSPソルバー
            └── SugarCspLangParser.scala - テキストファイルをScalaで読み込むためのパーサー
```

