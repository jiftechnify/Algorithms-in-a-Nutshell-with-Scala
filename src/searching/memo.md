# 探索
## 基本的な問題
1. 存在: 集合*C*の中に探索対象の要素があるか?
2. 関連照合: key-valueペアの集合から所望のkeyに対応するvalueを探す

## アルゴリズムの選択基準
|基準|アルゴリズム|
|:---|:----------|
|集合の要素数が少ない、集合に対し逐次アクセスしかできない|逐次探索|
|集合が変化せず、消費メモリ量を抑えたい|二分探索|
|集合が変化する|ハッシュに基づく探索・二分探索木|
|集合が変化する上に集合を整列順に処理することが多い|二分探索木|

