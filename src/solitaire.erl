-module(solitaire).
-author("Jarimatti Valkonen <jarimatti@me.com>").

%% For testing.
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%==============================================================================
%% Data Definitions

%%------------------------------------------------------------------------------
%% A Suit represents the suit of a playing card.
-type suit() :: spade | club | heart | diamond.


%%------------------------------------------------------------------------------
%% A Color is red or black.
-type color() :: red | black.

%%------------------------------------------------------------------------------
%% A Rank is the face value of a playing card.
-type rank() :: ace | 1..10 | jack | queen | king.


%%------------------------------------------------------------------------------
%% A Card has a suit and a value.
-type card() :: {rank(), suit()}.

%% Produce a new card from suit and rank.
-spec card(rank(), suit()) -> card().
card(Rank, Suit) ->
    {Rank, Suit}.

%% Produce the rank of the card.
-spec card_rank(card()) -> rank().
card_rank({Rank, _Suit}) ->
    Rank.

%% Produce the suit of the card.
-spec card_suit(card()) -> suit().
card_suit({_Rank, Suit}) ->
    Suit.


%%------------------------------------------------------------------------------
%% A Stock is a (possibly empty) sequence of cards.
-type stock() :: [card()].

%% Produce true if the stock is empty.
-spec stock_is_empty(stock()) -> boolean().
stock_is_empty([]) ->
    true;
stock_is_empty(_) ->
    false.


%%------------------------------------------------------------------------------
%% A Waste is a list of cards.
-type waste() :: [card()].


%%------------------------------------------------------------------------------
%% A Pile is a sequence of cards, where bottom cards can be face down.
-type pile() :: {[card()], [card()]} | lane.

%% Produce a new, empty pile.
-spec pile() -> pile().
pile() ->
    lane.

%% Produce a new pile from two lists of face-up and face-down cards.
-spec pile([card()], [card()]) -> pile().
pile([], []) ->
    lane;
pile([], [H | T]) ->
    {[H], T};
pile(U, D) ->
    {U, D}.

%% Produce the face up cards in the pile.
-spec pile_face_up(pile()) -> [card()].
pile_face_up({U, _D}) ->
    U.

%% Produce the face down cards in the pile.
-spec pile_face_down(pile()) -> [card()].
pile_face_down({_U, D}) ->
    D.

%% Produce a new pile with the sequence of cards on top.
-spec pile_place_sequence([card()], pile()) -> pile().
pile_place_sequence(Cs, lane) ->
    {Cs, []};
pile_place_sequence(Cs, {U, D}) ->
    {Cs ++ U, D}.

%% Produce a sequence of cards from pile.
-spec pile_sequence(non_neg_integer(), pile()) -> {[card()], pile()}.
pile_sequence(_X, lane) ->
    {[], lane};
pile_sequence(X, P = {U, _D}) when X > length(U) ->
    {[], P};
pile_sequence(X, {U, D}) ->
    {S, U1} = lists:split(X, U),
    {S, pile(U1, D)}.


%%------------------------------------------------------------------------------
%% A PileIndex is non_neg_integer.
-type pile_index() :: non_neg_integer().


%%------------------------------------------------------------------------------
%% A Tableau has 8 piles or lanes. A lane is an empty pile.
-type tableau() :: array:array(pile()).

%% Produce a new empty tableau with N lanes.
-spec tableau(pile_index()) -> tableau().
tableau(N) when is_integer(N), N > 0 ->
    array:new([N, fixed, {default, lane}]).

%% Produce a Pile from the Tableau.
-spec tableau_pile(pile_index(), tableau()) -> pile().
tableau_pile(N, T) ->
    array:get(N - 1, T).

%% Produce a new tableau with the pile N set to input.
-spec tableau_pile(pile_index(), pile(), tableau()) -> tableau().
tableau_pile(N, P, T) ->
    array:set(N - 1, P, T).


%%------------------------------------------------------------------------------
%% A Foundation is a sequence of cards of specific suit.
-type foundation() :: [card()].

%% Produce a new empty foundation.
-spec foundation() -> foundation().
foundation() ->
    [].


%%------------------------------------------------------------------------------
%% A Klondike game consists of Stock, Waste, 4 Foundations and a Tableau.
-record(klondike, {stock :: stock(),
                   waste :: waste(),
                   spades :: foundation(),
                   hearts :: foundation(),
                   diamonds :: foundation(),
                   clubs :: foundation(),
                   tableau :: tableau()}).

-type klondike() :: #klondike{}.

%% Produce the Stock of the Klondike.
-spec klondike_stock(klondike()) -> stock().
klondike_stock(#klondike{stock = S}) ->
    S.

%% Produce the Waste of the Klondike.
-spec klondike_waste(klondike()) -> waste().
klondike_waste(#klondike{waste = W}) ->
    W.

%% Produce a Klondike with given Waste.
-spec klondike_waste(waste(), klondike()) -> klondike().
klondike_waste(W, K) ->
    K#klondike{waste = W}.

%% Produce a Foundation of Suit from Klondike.
-spec klondike_foundation(suit(), klondike()) -> foundation().
klondike_foundation(spade, #klondike{spades = S}) ->
    S;
klondike_foundation(heart, #klondike{hearts = H}) ->
    H;
klondike_foundation(club, #klondike{clubs = C}) ->
    C;
klondike_foundation(diamond, #klondike{diamonds = D}) ->
    D.

%% Produce a new Klondike where the Foundation has been set.
-spec klondike_foundation(suit(), foundation(), klondike()) -> klondike().
klondike_foundation(spade, F, K) ->
    K#klondike{spades = F};
klondike_foundation(heart, F, K) ->
    K#klondike{hearts = F};
klondike_foundation(club, F, K) ->
    K#klondike{clubs = F};
klondike_foundation(diamond, F, K) ->
    K#klondike{diamonds = F}.


%%==============================================================================
%% Functions

%%------------------------------------------------------------------------------
%% Stock Functions.

%% Produce an ordered deck with all cards sorted according to suit and rank.
-spec deck() -> [card()].
deck() ->
    [ card(R, S) ||
        S <- [spade, heart, club, diamond],
        R <- lists:flatten([ace,
                            lists:seq(2,10),
                            jack, queen, king])
    ].


%%------------------------------------------------------------------------------
%% Klondike Functions.

%% Produce a new Klondike game given the (shuffled) deck.
-spec klondike([card()]) -> klondike().
klondike(D) ->
    {T, S} = make_tableau(D),
    #klondike{stock = S,
              waste = [],
              spades = [],
              hearts = [],
              clubs = [],
              diamonds = [],
              tableau = T}.


%% Produce the Tableau of the Klondike.
-spec klondike_tableau(klondike()) -> tableau().
klondike_tableau(#klondike{tableau = T}) ->
    T.


%% Produce a Klondike with specified Tableau.
-spec klondike_tableau(tableau(), klondike()) -> klondike().
klondike_tableau(T, K) ->
    K#klondike{tableau = T}.


%% Produce initial Tableau and remaining Stock given a list of cards.
-spec make_tableau([card()]) -> {tableau(), stock()}.
make_tableau(D) ->
    T = tableau(7),
    make_tableau(D, T, 1, 7).

%% Helper for make_tableau
make_tableau(D, T, N, X) when N > X ->
    {T, D};
make_tableau(D, T, N, X) ->
    {[D1H | D1T], D2} = lists:split(N, D),
    T1 = tableau_pile(N, pile([D1H], D1T), T),
    make_tableau(D2, T1, N + 1, X).


%%------------------------------------------------------------------------------
%% Turn cards from stock to waste.

%% Produce a new Klondike where three cards have been turned from stock.
-spec turn_from_stock(klondike()) -> klondike().
turn_from_stock(K = #klondike{stock = S, waste = W}) ->
    {S1, W1} = stock_to_waste(S, W),
    K#klondike{stock = S1, waste = W1}.


%% Produce {Stock, Waste} where 3 cards from stock are in Waste.
-spec stock_to_waste(stock(), waste()) -> {stock(), waste()}.
stock_to_waste([S1, S2, S3 | S], W) ->
    {S, [S3, S2, S1 | W]};
stock_to_waste([S1, S2], W) ->
    {[], [S2, S1 | W]};
stock_to_waste([S1], W) ->
    {[], [S1 | W]};
stock_to_waste([], W) ->
    {[], W}.


%%------------------------------------------------------------------------------
%% Move a card from Waste directly to Foundation pile, if possible.

%% Produce a new Klondike where a waste card is moven to Foundation pile.
%% If not possible, produces the initial game unchanged.
-spec move_waste_to_foundation(klondike()) -> klondike().
move_waste_to_foundation(K) ->
    move_waste_to_foundation(K, klondike_waste(K)).

-spec move_waste_to_foundation(klondike(), waste()) -> klondike().
move_waste_to_foundation(K, []) ->
    K;
move_waste_to_foundation(K, [C | T]) ->
    F = klondike_foundation(card_suit(C), K),
    case can_move_to_foundation(C, F) of
        true ->
            K1 = klondike_foundation(card_suit(C), [C | F], K),
            klondike_waste(T, K1);
        false ->
            K
    end.


%% Produce true if the card can be moved to foundation.
%% ASSUME: card and foundation are of the same suit.
-spec can_move_to_foundation(card(), foundation()) -> boolean().
can_move_to_foundation(_C, []) ->
    true;
can_move_to_foundation(C, [F | _T]) ->
    rank_succ(card_rank(F)) == card_rank(C).


%% Produce the successor to the Rank. King has no successor.
-spec rank_succ(rank()) -> rank().
rank_succ(ace) ->
    2;
rank_succ(N) when N >= 2, N < 10 ->
    N + 1;
rank_succ(10) ->
    jack;
rank_succ(jack) ->
    queen;
rank_succ(queen) ->
    king.


%%------------------------------------------------------------------------------
%% Move a card from Tableau Pile to Foundation.

%% Produce a Klondike where the card from Tableau Pile is moved to Foundation.
-spec move_card_to_foundation(pile_index(), klondike()) -> klondike().
move_card_to_foundation(I, K) ->
    T = klondike_tableau(K),
    mcf(tableau_remove_top_card(I, T), K).

mcf({none, _T}, K) ->
    K;
mcf({C, T}, K) ->
    F = klondike_foundation(card_suit(C), K),
    case can_move_to_foundation(C, F) of
        true ->
            K1 = klondike_foundation(card_suit(C), [C | F], K),
            klondike_tableau(T, K1);
        false ->
            K
    end.


%% Produce the top card and new Tableau with top card removed from pile.
-spec tableau_remove_top_card(pile_index(), tableau()) -> {card() | none, tableau()}.
tableau_remove_top_card(I, T) ->
    {C, P} = pile_remove_top_card(tableau_pile(I, T)),
    {C, tableau_pile(I, P, T)}.


%% Produce the top card (or none) and the a new pile by removint topmost card.
-spec pile_remove_top_card(pile()) -> {card() | none, pile()}.
pile_remove_top_card(lane) ->
    {none, lane};
pile_remove_top_card({[H], []}) ->
    {H, lane};
pile_remove_top_card({[H], [DH | DT]}) ->
    {H, {[DH], DT}};
pile_remove_top_card({[H | T], D}) ->
    {H, {T, D}}.


%%------------------------------------------------------------------------------
%% Move a sequence of cards from one pile to another.


%% Produce a Klondike where a sequence has been moved from one pile to another.
%% Do nothing if the move is not legal.
-spec move_sequence({pile_index(), non_neg_integer()},
                    pile_index(), klondike()) -> klondike().
move_sequence(From, To, K) ->
    T = tms(From, To, klondike_tableau(K)),
    klondike_tableau(T, K).

tms({FP, FC}, TP, T) ->
    Pa = tableau_pile(FP, T),
    Pb = tableau_pile(TP, T),
    {S, Pa1} = pile_sequence(FC, Pa),
    case is_sequence(S) and can_move(S, Pb) of
        true ->
            Pb1 = pile_place_sequence(S, Pb),
            tableau_pile(FP, Pa1, tableau_pile(TP, Pb1, T));
        false ->
            T
    end.
    

%% Produce true if the sequence is not empty and valid.
is_sequence([]) ->
    false;
is_sequence([H | T]) ->
    F = fun (A, {B, true}) ->
                CM = cm(color(card_suit(A)), color(card_suit(B))),
                VM = card_rank(A) == rank_succ(card_rank(B)),
                {A, CM and VM};
            (_, {C, false}) ->
                {C, false}
        end,
    {_C, R} = lists:foldl(F, {H, true}, T),
    R.

-ifdef(TEST).

is_sequence_empty_test() ->
    false = is_sequence([]).

is_sequence_invalid_color_test() ->
    false = is_sequence([card(3, spade),
                         card(4, heart),
                         card(5, diamond)]).

is_sequence_invalid_rank_test() ->
    false = is_sequence([card(3, spade),
                         card(4, heart),
                         card(3, club)]).

is_sequence_valid_test() ->
    true = is_sequence([card(3, spade),
                        card(4, heart),
                        card(5, club),
                        card(6, diamond)]).

-endif.


%% Produce true if the sequence can be moved on top of the pile.
can_move([], _) ->
    false;
can_move(S, lane) ->
    king == card_rank(lists:last(S));
can_move(S, P) ->
    A = lists:last(S),
    B = hd(pile_face_up(P)),
    CM = cm(color(card_suit(A)), color(card_suit(B))),
    RM = rank_succ(card_rank(A)) == card_rank(B),
    CM and RM.

-ifdef(TEST).

can_move_empty_seq_test() ->
    false = can_move([], lane).

can_move_king_to_lane_test() ->
    true = can_move([card(king, heart)], lane).

can_move_other_to_lane_test() ->
    false = can_move([card(5, heart)], lane).

can_move_valid_test() ->
    true = can_move([card(5, heart)], pile([card(6, spade)], [])).

can_move_invalid_color_test() ->
    false = can_move([card(5, heart)], pile([card(6, heart)], [])).

can_move_invalid_rank_test() ->
    false = can_move([card(5, heart)], pile([card(4, spade)], [])).

-endif.


%% Produce true if the colors are different.
-spec cm(color(), color()) -> boolean().
cm(black, red) ->
    true;
cm(red, black) ->
    true;
cm(_, _) ->
    false.


%% Produce suite color.
-spec color(suit()) -> color().
color(spade) ->
    black;
color(club) ->
    black;
color(heart) ->
    red;
color(diamond) ->
    red.


%%------------------------------------------------------------------------------
%% Move a card from Waste to Tableau Pile.

%% Produce Klondike, where a card has been moved to Pile from Waste.
-spec waste_to_pile(pile_index(), klondike()) -> klondike().
waste_to_pile(I, K) ->
    case klondike_waste(K) of
        [] ->
            K;
        [H | T] ->
            S = [H],
            KT = klondike_tableau(K),
            P = tableau_pile(I, KT),
            case can_move(S, P) of
                true ->
                    P1 = pile_place_sequence(S, P),
                    klondike_waste(
                      T,
                      klondike_tableau(tableau_pile(I, P1, KT), K));
                false ->
                    K
            end
    end.


%%------------------------------------------------------------------------------
%% Simple Console I/O.

%% solitaire:print_klondike(solitaire:klondike(solitaire:deck())).

-spec print_klondike(klondike()) -> ok.
print_klondike(K) ->
    S = length(klondike_stock(K)),
    W = format_card(klondike_waste(K)),
    F1 = format_card(klondike_foundation(spade, K)),
    F2 = format_card(klondike_foundation(heart, K)),
    F3 = format_card(klondike_foundation(club, K)),
    F4 = format_card(klondike_foundation(diamond, K)),
    T = format_tableau(klondike_tableau(K)),
    io:format("    [~3b] [~3s]       [~3s] [~3s] [~3s] [~3s]~n", [S, W, F1, F2, F3, F4]),
    io:format("~n"),
    io:format("~s~n", [T]).


format_card([]) ->
    "";
format_card([H | _T]) ->
    format_card(H);
format_card(C) ->
    lists:flatten([format_rank(card_rank(C)), format_suit(card_suit(C))]).


format_rank(ace) ->
    $A;
format_rank(jack) ->
    $J;
format_rank(queen) ->
    $Q;
format_rank(king) ->
    $K;
format_rank(X) when is_integer(X), X >= 2, X =< 10 ->
    io_lib:format("~b", [X]).


format_suit(spade) ->
    $s;
format_suit(heart) ->
    $h;
format_suit(club) ->
    $c;
format_suit(diamond) ->
    $d.


format_tableau(T) ->
    A = format_face_down_counts(T),
    B = format_face_up_cards(T),
    lists:flatten(A ++ B).


format_face_down_counts(T) ->
    L = lists:map(fun (lane) ->
                          0;
                      (P) ->
                          length(pile_face_down(P))
                  end,
                  array:to_list(T)),
    io_lib:format("    [~3b] [~3b] [~3b] [~3b] [~3b] [~3b] [~3b]~n", L).
    

format_face_up_cards(T) ->
    format_face_up_cards(T, max_pile_length(T), "").


format_face_up_cards(_T, 0, Acc) ->
    lists:reverse(Acc);
format_face_up_cards(T, X, Acc) ->
    format_face_up_cards(T, X - 1, [fpiles(T, X) | Acc]).


fpiles(T, X) ->
    L = lists:map(fun (lane) ->
                          "";
                      (P) ->
                          U = pile_face_up(P),
                          case X =< length(U) of
                              true ->
                                  format_card(lists:nth(X, U));
                              false ->
                                  ""
                      end
                  end,
                  array:to_list(T)),
    io_lib:format("~2b: [~3s] [~3s] [~3s] [~3s] [~3s] [~3s] [~3s]~n", [X | L]).


max_pile_length(T) ->
    lists:max(
      lists:map(fun (lane) ->
                        0;
                    (P) ->
                        length(pile_face_up(P))
                end,
                array:to_list(T))).


mf(waste, K) ->
    move_waste_to_foundation(K);
mf(P, K) ->
    move_card_to_foundation(P, K).

mc({P1, R}, P2, K) ->
    move_sequence({P1, R}, P2, K).

