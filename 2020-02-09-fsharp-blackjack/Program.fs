module rec Blackjack

open System
open System.Collections.Generic     // IList<> のため
open System.Threading               // ThreadLocal のため

// -----------------------------------------------
// カードの型、生成、名前
// -----------------------------------------------

/// カードのランク(数字)の型
///
/// 設計: ブラックジャックではエースやキングが重要な意味を持つので、
///      int ではなく判別共用体を使う方が書きやすい。
///      ケースを書く順番が値の大小関係になるので、小さい順に並べている。
type Rank =
    | Ace

    /// 2〜10
    | Rank of rank:int

    | Jack

    | Queen

    | King

let allRanks () =
    [
        Ace
        Rank 2
        Rank 3
        Rank 4
        Rank 5
        Rank 6
        Rank 7
        Rank 8
        Rank 9
        Rank 10
        Jack
        Queen
        King
    ]

let rankToName rank =
    match rank with
    | Ace       -> "エース"
    | Rank n    -> string n
    | Jack      -> "ジャック"
    | Queen     -> "クイーン"
    | King      -> "キング"

/// カードのスート(柄)の型
///
/// 設計ノート: ブラックジャックではスートが意味を持たないので、文字列で OK。
///           後から判別共用体にするのも難しくないはず。
type Suit = string

let allSuits () =
    [
        "スペード"
        "クローバー"
        "ハート"
        "ダイヤ"
    ]

/// カードの型
///
/// 設計ノート: ブラックジャックではジョーカーは使わない。
type Card =
    | Card of suit:Suit * rank:Rank

let allCards () =
    [
        for suit in allSuits () do
            for rank in allRanks () do
                Card (suit, rank)
    ]

let cardToName card =
    match card with
    | Card (suit, rank) ->
        suit + "の" + rankToName rank

// 枚数を確認。
assert (List.length (allCards ()) = 13 * 4)

// -----------------------------------------------
// デックの型、生成、ドロー
// -----------------------------------------------

/// 乱数生成器 (スレッドごとに1つ)
let sRandom = new ThreadLocal<Random>(fun () -> Random())

/// 参考: フィッシャー・イェーツのシャッフルアルゴリズム
let shuffleArray (list: IList<Card>) =
    let random = sRandom.Value

    let n = list.Count
    for i in n - 1..-1..1 do // i=n-1 から i=1 まで降るループ
        let j = random.Next(0, i + 1)

        // i, j 番目を交換する。
        let t = list.[i]
        list.[i] <- list.[j]
        list.[j] <- t

type Deck = ResizeArray<Card>

/// デックを生成する。
let generateDeck () =
    let deck = ResizeArray(allCards ())
    shuffleArray deck
    deck

/// カードを1枚引く。
let drawFromDeck (deck: Deck) =
    /// ※現在の実装では、カードが尽きることはない。
    assert (deck.Count >= 1)

    let last = deck.Count - 1
    let card = deck.[last]
    deck.RemoveAt(last)

    card

// -----------------------------------------------
// 手札
// -----------------------------------------------

/// プレイヤーの手札
type Hand = ResizeArray<Card>

let newHand () = ResizeArray()

let addToHand card (yourHand: Hand) =
    yourHand.Add(card)

/// ディーラーの初期手札。2枚目は裏向き(faced down)。
type DealerHand =
    | DealerHand of facedUp:Card * facedDown:Card

let dealerHandToFacedDownCard dealerHand =
    match dealerHand with
    | DealerHand (_, card) ->
        card

let dealerHandToHand dealerHand =
    match dealerHand with
    | DealerHand (card1, card2) ->
        let cards = [card1; card2] // リストの区切りはセミコロンなので注意！！
        ResizeArray(cards)

// -----------------------------------------------
// スコアの計算
// -----------------------------------------------

let rankToScore rank =
    match rank with
    | Ace ->
        1 // ※現在のルールでは A=1 固定

    | Rank n ->
        n

    | Jack
    | Queen
    | King ->
        10

let cardToScore card =
    match card with
    | Card (_suit, rank) ->
        rankToScore rank

let handToScore (hand: Hand) =
    Seq.sumBy cardToScore hand

// -----------------------------------------------
// ゲームの勝敗
// -----------------------------------------------

type GameResult =
    | YouWin
    | YouLose

let scoreIsBust score =
    score > 21

let handsToGameResult yourHand dealersHand =
    let yourScore = handToScore yourHand
    let dealersScore = handToScore dealersHand

    let youWin =
        scoreIsBust dealersScore || (
            not (scoreIsBust yourScore)
            && yourScore > dealersScore
        )

    if youWin then YouWin else YouLose

// -----------------------------------------------
// ディーラーの思考
// -----------------------------------------------

type DealerAction =
    | DealerHitAction
    | DealerStandAction

let scoreToDealerAction (score: int) =
    assert (not (scoreIsBust score))

    if score <= 16 then
        DealerHitAction
    else
        DealerStandAction

let handToDealerAction (hand: Hand) =
    scoreToDealerAction (handToScore hand)

// -----------------------------------------------
// ゲーム進行
// -----------------------------------------------

type Phase =
    | GameStartPhase

    | YourActionPhase
        of Deck * Hand * DealerHand

    | DealerOpenPhase
        of Deck * Hand * DealerHand

    | DealerActionPhase
        of Deck * Hand * Hand

    | GameEndPhase
        of GameResult

/// エンターキーを待つ。ゲームが早く進みすぎないようにするため。
let wait () =
    printfn "----"
    stdin.ReadLine() |> ignore

/// ユーザーの yes/no を待つ。
let confirm () =
    match stdin.ReadLine() with
    | "y" | "Y" ->
        true

    | _ ->
        false

let youHit (deck: Deck) (yourHand: Hand) =
    let card = drawFromDeck deck
    addToHand card yourHand
    printfn "あなたは %s を引きました。" (cardToName card)

let gameStart () =
    printfn "☆★☆★ ブラックジャックにようこそ！ ★☆★☆"
    printfn "ゲームを開始します。"
    wait ()

    let deck = generateDeck ()

    let yourHand = newHand ()
    youHit deck yourHand
    youHit deck yourHand

    let facedUp = drawFromDeck deck
    let facedDown = drawFromDeck deck
    let dealersHand = DealerHand (facedUp, facedDown)
    printfn "ディーラーの1枚目のカードは %s です。" (cardToName facedUp)
    printfn "ディーラーの2枚目のカードは裏向きです。"
    wait ()

    doYourActionPhase (deck, yourHand, dealersHand)

let doYourActionPhase (deck, yourHand, dealersHand) =
    let score = handToScore yourHand
    printfn "あなたの現在の得点は %d です。" score

    if scoreIsBust score then
        printfn "バストしてしまいました……"
        wait ()

        gameEnd YouLose
    else

    printfn "ヒットしますか？ (カードを引くなら Y、引かないなら N)"
    if not (confirm ()) then
        doDealerOpenPhase (deck, yourHand, dealersHand)
    else

    youHit deck yourHand
    doYourActionPhase (deck, yourHand, dealersHand)

let doDealerOpenPhase (deck, yourHand, dealersHand) =
    let facedDownCard = dealerHandToFacedDownCard dealersHand
    printfn "ディーラーの2枚目のカードは %s でした。" (cardToName facedDownCard)
    wait ()

    let dealersHand = dealerHandToHand dealersHand
    doDealerActionPhase (deck, yourHand, dealersHand)

let doDealerActionPhase (deck, yourHand, dealersHand) =
    let score = handToScore dealersHand
    printfn "ディーラーの現在の得点は %d です。" score

    if scoreIsBust score then
        printfn "ディーラーがバストしました🔥"
        wait ()

        gameEnd YouWin
    else

    match scoreToDealerAction score with
    | DealerHitAction ->
        printfn "ディーラーがヒットしました"
        wait ()

        let card = drawFromDeck deck
        addToHand card dealersHand
        printfn "ディーラーが引いたカードは %s です。" (cardToName card)
        doDealerActionPhase (deck, yourHand, dealersHand)

    | DealerStandAction ->
        printfn "ディーラーがスタンドしました✋"
        wait ()

        let gameResult = handsToGameResult yourHand dealersHand
        gameEnd gameResult

let gameEnd result =
    match result with
    | YouWin ->
        printfn "あなたの勝ちです！ おめでとう🎉"

    | YouLose ->
        printfn "あなたの負けです。どんまい♪"

    printfn "ブラックジャック終了！ また遊んでね★"

[<EntryPoint>]
let main _ =
    gameStart ()
    0
