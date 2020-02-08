module Program

// これは F# の良さを示すための、可能なかぎり小さいサンプルです。

// -----------------------------------------------
// (1/3) 網羅的パターンマッチ
// -----------------------------------------------

// カードの柄 (スート) を表す Suit 型を「判別共用体」として定義します。
// ここでは C# の enum とほぼ同様です。
type Suit =
    | Spade
    | Clover
    | Heart
    | Diamond

// 例として、スートの名前を取得する関数を定義します。
let suitToName suit =
    match suit with
    | Spade     -> "スペース"
    | Clover    -> "クローバー"
    | Heart     -> "ハート"
    | Diamond   -> "ダイアモンド"

//  ↑  match は switch のように場合分けの構文です。ただし default 節はいりません。
//     場合分けに漏れがあるときは、コンパイルエラーになります。

// 実行例
printfn "Spade: %s" (suitToName Spade)
    //=> スペード

// -----------------------------------------------
// (2/3) 値を持つ判別共用体
// -----------------------------------------------

// トランプのカードを表す Card 型を、「判別共用体」で定義します。
// 判別共用体の各ケースは、C# の enum と違って、フィールドを持つことができます。
type Card =
    | NormalCard of suit:Suit * rank:int
    | Joker

    // ↑ of の後ろがフィールドの定義です。
    //   普通のカード(NormalCard)はスート(柄)とランク(数字)を持ちますが、
    //   Joker は特に何も持ちません。

// 値を持つケースは、コンストラクタを使ってインスタンス化できます。(new がないですが、コンストラクタです。)
let heart3 = NormalCard (Heart, 3)

// 使用例として、カードの名前を取得する関数を定義します。
let cardToName card =
    match card with
    | Joker ->
        "ジョーカー"

    | NormalCard (suit, rank) ->
        let suitName = suitToName suit
        let rankName = string rank
        suitName + "の" + rankName

// 実行例
printfn "Joker: %s" (cardToName Joker)
    //=> ジョーカー

printfn "Heart3: %s" (cardToName heart3)
    //=> ハートの3

// 「判別共用体」の実用的な例は、いくらでもあります。

type Login =
    | EmailLogin of address:string
    | TwitterLogin of accessToken:string * accessSecret:string

// -----------------------------------------------
// (3/3) 型推論
// -----------------------------------------------

// ところで、ここまでに定義した関数の引数や結果には、型を全く書きませんでした。
// しかし、動的検査というわけではありません。
// もし型エラーがあれば、コンパイルエラーとして報告されます。

// ↓ 信じられない人は、これをコンパイルしてみてください。
// printfn "%s" (cardToName "ジョーカー")

// -----------------------------------------------
// おわり
// -----------------------------------------------

// これは Main() 関数の代わりです。
[<EntryPoint>]
let main _ = 0
