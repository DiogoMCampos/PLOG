:-use_module(library(lists)).
:-use_module(library(random)).
:-include('interface.pl').

boardSize(9).
boardStartIndex(1).

convertLetterToIndex(Column, [X|Xs], Index, Result) :-
    Column \== X ->
        NewIndex is Index+1,
        convertLetterToIndex(Column, Xs, NewIndex, Result);
    returnResult(Index, Result).

withinBoard(X, Y, Size) :-
    X > 0,
    X =< Size,
    Y > 0,
    Y =< Size.

isOrthogonal(InC, InL, DeC, DeL, HorMove, VertMove, Amount) :-
    (InC == DeC ->
        HorMove is 0,
        (InL > DeL ->
            Amount is InL - DeL,
            VertMove is -1
        ;InL < DeL ->
            Amount is DeL - InL,
            VertMove is 1))
    ;(InL == DeL ->
        VertMove is 0,
        (InC > DeC ->
            Amount is InC - DeC,
            HorMove is -1
        ;InC < DeC ->
            Amount is DeC - InC,
            HorMove is 1)).

returnResult(Result, Result).

getListElement(Index, [X|Xs], Iterator, Result) :-
    Index =\= Iterator ->
        NewIterator is Iterator+1,
        getListElement(Index, Xs, NewIterator, Result)
    ;   returnResult(X,Result).

isPiece(piece(_,_)).

getPiece([X|Xs], ColumnIndex, LineIndex, Piece) :-
    getListElement(10-LineIndex, [X|Xs], 1, Line),
    getListElement(ColumnIndex, Line, 1, Piece),
    isPiece(Piece).

pushOpponents([], _) :- fail.
pushOpponents([_-_-Color-_|Rest],  Player) :-
    (Color \== Player ->
        true
    ; pushOpponents(Rest, Player)).

housesAffected(_, _, _, _, _, 0, Affected, Affected, _).
housesAffected([X|Xs], Column, Line, HorMove, VertMove, Amount, Affected, Total, [Pieces|Rest]) :-
    boardSize(Size),
    withinBoard(Column, Line, Size) ->
        NewColumn is Column + HorMove * Amount,
        NewLine is Line + VertMove * Amount,
        (getPiece([X|Xs], Column, Line, Object) ->
            ((Affected - 1) >= 0 ->
                NewAffected is Affected - 1,
                NewAmount is Amount,
                pieceColor(Object, Color),
                returnResult(Pieces, Column-Line-Color-Amount),
                housesAffected([X|Xs], NewColumn, NewLine, HorMove, VertMove, NewAmount, NewAffected, Total, Rest)
            ;!,fail)
        ;   NewAmount is Amount - 1,
            NewAffected is Affected,
            housesAffected([X|Xs], NewColumn, NewLine, HorMove, VertMove, NewAmount, NewAffected, Total, [Pieces|Rest]))
    ;   Total is Affected.

/* Saves all pieces coordinates from one player still in the game in a list. Returns num remaining pieces too*/
getPiecesCoordinates(Board, Column, Line, Side, [PieceCoords|Locals], PiecesLeft, PiecesTotal) :-
    boardSize(Size),
    (Column + 1 > Size ->
        NewCol is 1,
        NewLine is Line + 1
    ;   NewCol is Column + 1,
        NewLine is Line),

    (Line =< Size ->
        (getPiece(Board, Column, Line, Piece) ->
            pieceColor(Piece, Color),
            (Color == Side ->
                pieceHeight(Piece,Height),
                returnResult(PieceCoords, Column-Line-Height),
                NewPiecesLeft is PiecesLeft + 1,
                getPiecesCoordinates(Board, NewCol, NewLine, Side, Locals, NewPiecesLeft, PiecesTotal)
            ;   getPiecesCoordinates(Board, NewCol, NewLine, Side, [PieceCoords|Locals], PiecesLeft, PiecesTotal))
        ;   getPiecesCoordinates(Board, NewCol, NewLine, Side, [PieceCoords|Locals], PiecesLeft, PiecesTotal))
    ;   returnResult(PiecesLeft, PiecesTotal)).

abc(X) :- boardMidGame(Board), getPiecesCoordinates(Board, 1,1,X,Coords, 0, Pieces), write(Coords),nl,write(Pieces).

a(A,B,Y,Z) :-
    boardStart(X),
    housesAffected(X, A, B, Y, Z, 3, 4, InvTotal, [_|Rest]), reverse(Rest, [_|Real]),write(Real),
    Total is 4 - InvTotal,
    nl, write(Total).

getFirstValuePair(Value-_, Value).
getSecondValuePair(_-Value, Value).

addToList(Element, List, [Element|List]).

getOutsideBoard([], _).
getOutsideBoard([Pair|PairRest], [Removed|RemovedRest]) :-
    getFirstValuePair(Pair, Place),
    Place > 9 ->
        getSecondValuePair(Pair, Piece),
        Removed = Piece,
        getOutsideBoard(PairRest, RemovedRest)
    ;   getOutsideBoard(PairRest, [Removed|RemovedRest]).

createOriginPair(Column-_-Amount, Column-Amount).
createDestPair(Column-Piece, Column, Piece).

createMovementPairs([],[]).
createMovementPairs([Move|Rest], [P|Ps]) :-
    createOriginPair(Move, P),
    createMovementPairs(Rest, Ps).

getMoveLine([_-Line-_|_], Line).

deletePieces(_, _, _, [], Pieces, Pieces, 0).
deletePieces(X, CurrC, Direction, [Piece|Rest], Pieces, NewPieces, Deleted) :-
    getFirstValuePair(Piece, Column),
    CurrC == Column ->
        getSecondValuePair(Piece, Amount),
        (Direction > 0 ->
            DestC is Column + Amount
        ;   DestC is Column - Amount),
        createDestPair(Pair, DestC, X),
        addToList(Pair, Pieces, NewPieces),
        Deleted is 1
    ;   deletePieces(X, CurrC, Direction, Rest, Pieces, NewPieces, Deleted).

addPieces(_, o, _, [], 1).
addPieces(X, X, _, [], 0).
addPieces(X, N, CurrC, [Piece|Rest], Deleted) :-
    getFirstValuePair(Piece, Column),
    CurrC == Column ->
        getSecondValuePair(Piece, Object),
        N = Object
    ;   addPieces(X, N, CurrC, Rest, Deleted).

moveLine([],_,_,_,_, Pieces, Pieces).
moveLine([X|Xs], [N|Ns], CurrC, Direction, PiecesToMove, Pieces, Removed) :-
    deletePieces(X, CurrC, Direction, PiecesToMove, Pieces, NewPieces, Deleted),
    addPieces(X, N, CurrC, NewPieces, Deleted),
    (Direction > 0 ->
        NextC is CurrC + 1
    ;   NextC is CurrC - 1),
    moveLine(Xs, Ns, NextC, Direction, PiecesToMove, NewPieces, Removed).

moveHorAuxiliar(_,_,_,_,0,_,_).
moveHorAuxiliar([X|Xs], [N|Ns], MovePairs, MoveLine, CurrLine, Direction, Removed) :-
    (CurrLine == MoveLine ->
        (Direction > 0 ->
            moveLine(X, N, 1, Direction, MovePairs, [], Removed)
        ;   reverse(X, TempLine),
            moveLine(TempLine, TempN, 9, Direction, MovePairs, [], Removed),
            reverse(TempN, N))
    ;   returnResult(X, N)),
    NextLine is CurrLine - 1,
    moveHorAuxiliar(Xs, Ns, MovePairs, MoveLine, NextLine, Direction, Removed).

moveHorizontal([X|Xs], [N|Ns], MoveList, Direction, Removed) :-
    createMovementPairs(MoveList, Pairs),
    getMoveLine(MoveList, MoveLine),
    moveHorAuxiliar([X|Xs], [N|Ns], Pairs, MoveLine, 9, Direction, MovedList),
    getOutsideBoard(MovedList, Outside), reverse(Outside, [_|Removed]).

moveList([5-2-3, 4-2-3, 3-2-3]).

test2(Removed) :-
    boardStart(X),
    moveList(Y),
    moveHorizontal(X, N, Y, -1, Removed),
    displayBoard(N, 9, 9).

moveVerAuxiliar(_,_,_,_,0,_,_).
moveVerAuxiliar([X|Xs], [N|Ns], InC, InL, CurrLine, DeL, Piece) :-
    (   CurrLine == InL -> removeFromLine(X, N, InC, 1)
    ;   CurrLine == DeL -> addToLine(X, N, InC, 1, Piece)
    ;   returnResult(X, N)),
    NextLine is CurrLine - 1,
moveVerAuxiliar(Xs, Ns, InC, InL, NextLine, DeL, Piece).

moveVertical([X|Xs], [N|Ns], InC, InL, DeL) :-
    getPiece([X|Xs], InC, InL, Piece),
    moveVerAuxiliar([X|Xs], [N|Ns], InC, InL, 9, DeL, Piece).

/* Missing Player and [X|Xs] and board size is currently hardcoded */
verifyMove(Board,InC, InL, DeC, DeL, HorMove, VertMove, Player, PiecesAffected) :-
    boardSize(Size),
    withinBoard(InC, InL, Size),
    withinBoard(DeC, DeL, Size),
    isOrthogonal(InC, InL, DeC, DeL, HorMove, VertMove, Amount),
    getPiece(Board, InC, InL, Piece),
    pieceColor(Piece, Color),
    Color == Player,
    pieceHeight(Piece, Height),
    Height >= Amount,
    housesAffected(Board, InC, InL, HorMove, VertMove, Amount, Height + 1, InverseTotal, InvertedAffected),
    reverse(InvertedAffected, [_|PiecesAffected]),!,
    TotalAffected is Height + 1 -InverseTotal,
    (TotalAffected == 1 -> true
    ;pushOpponents(PiecesAffected, Player)).

ab(X,Y,X1,Y1, Side) :- boardMidGame(Board), verifyMove(Board, X,Y,X1,Y1, Side,A,B), write(A),nl,write(B).

listPossible(Column, Line, HorMove, VertMove, Amount, [Move|Rest]) :-
    Amount > 0 ->
        NewAmount is Amount-1,
        DestCol is Column + HorMove * Amount,
        DestLin is Line + VertMove * Amount,
        returnResult(Column-Line-DestCol-DestLin, Move),
        listPossible(Column, Line, HorMove, VertMove, NewAmount, Rest)
    ;   true.

getPossibleDirection(_, _, _, _, _, 0, _, Total, Moves) :- Total is 0, returnResult(Moves, []).
getPossibleDirection(Board, Column, Line, HorMove, VertMove, Amount, Affected, Total, Moves) :-
    DestCol is Column + (HorMove * Amount),
    DestLin is Line + (VertMove * Amount),
    boardSize(Size),
    ((withinBoard(DestCol, DestLin, Size),
    housesAffected(Board, Column, Line, HorMove, VertMove, Amount, Affected, _, _)) ->
        Total is Amount,
        listPossible(Column, Line, HorMove, VertMove, Amount, Moves)
    ;getPossibleDirection(Board, Column, Line, HorMove, VertMove, Amount-1, Affected, Total, Moves)).

possibleMoves(Board, Column, Line, Total, Possible) :-
    getPiece(Board, Column, Line, Piece),
    pieceHeight(Piece, Height),
    getPossibleDirection(Board, Column, Line, -1,  0, Height, Height+1, Total1, Inverted1),
    getPossibleDirection(Board, Column, Line,  1,  0, Height, Height+1, Total2, Inverted2),
    getPossibleDirection(Board, Column, Line,  0, -1, Height, Height+1, Total3, Inverted3),
    getPossibleDirection(Board, Column, Line,  0,  1, Height, Height+1, Total4, Inverted4),
    Total is Total1 + Total2 + Total3 + Total4,
    reverse(Inverted1, [_|Moves1]),
    reverse(Inverted2, [_|Moves2]),
    reverse(Inverted3, [_|Moves3]),
    reverse(Inverted4, [_|Moves4]),
    append(Moves1, Moves2, SumMoves1),
    append(Moves3, Moves4, SumMoves2),
    append(SumMoves1, SumMoves2, Possible).

listAllMoves(_, [], TotalMoves, TotalMoves, Moves, Moves).
listAllMoves(Board, [Column-Line-_|Rest], NumTotal, TotalMoves, Moves, AllMoves) :-
    possibleMoves(Board, Column, Line, NumPossible, Possible),
    (NumPossible > 0 ->
        NewTotal is NumTotal + NumPossible,
        append(Moves, Possible, NewMoves),
        listAllMoves(Board, Rest, NewTotal, TotalMoves, NewMoves, AllMoves)
    ;   listAllMoves(Board, Rest, NumTotal, TotalMoves, NewMoves, AllMoves)).

allPossibleMoves(Board, Side, Total, Moves) :-
    getPiecesCoordinates(Board, 1, 1, Side, Over, 0, _),
    reverse(Over, [_|Coords]),
    listAllMoves(Board, Coords, 0, Total, [], Moves).

a(X,Y) :- boardStart(Z), possibleMoves(Z, X, Y, Total, Moves), write(Total), nl, write(Moves).
b(X,Y) :- boardStart(Z), getPossibleDirection(Z, X, Y, -1, 0, 3,4,Total, Moves), write(Total), nl, write(Moves).
c(X) :- boardStart(Z), allPossibleMoves(Z, r, X,Y), write(Y).
d(X, InC, InL, DeC, DeL) :- boardStart(Z), generateRandomMove(Z, X, InC, InL, DeC, DeL).

generateRandomMove(Board, Side, InC, InL, DeC, DeL) :-
    allPossibleMoves(Board, Side, NumMoves,AllMoves),
    random(0, NumMoves, Option),!,
    write(NumMoves+Option),nl,
    write(AllMoves),
    getListElement(Option, AllMoves, 0, InC-InL-DeC-DeL),
    write(InC+InL+DeC+DeL), nl.

analyseMove(Board, Player, HorMove, VertMove, PiecesAffected) :-
    askMove(InC, InL, DeC, DeL),
    letters(A),
    convertLetterToIndex(InC, A, 1, InColInd),
    convertLetterToIndex(DeC, A, 1, DeColInd),
    verifyMove(Board, InColInd, InL, DeColInd, DeL, HorMove, VertMove,Player, PiecesAffected).

finish(P1Color-P1Points,P1Color-P2Points,[], P1Points, P2Points, Side) :-
    P2Points >= 7 ->
        returnResult(P2Color, Side)
    ;P1Points >=7 ->
        returnResult(P1Color, Side).
finish(P1Color-P1Points, P2Color-P2Points, [Removed|Rest], NewP1Points, NewP2Points) :-
    pieceColor(Removed,Color),
    pieceHeight(Removed,Height),
    (Color == P1Color ->
        NewPoints is P1Points+Height,
        finish(P1Color-NewPoints, P2Color-P2Points, Rest, NewP1Points, NewP2Points)
    ;   NewPoints is P2Points+Height,
        finish(P1Color-P1Points, P2Color-NewPoints, Rest, NewP1Points, NewP2Points)).

vsComputer(Board, Player, Computer):-
    (analyseMove(Board, Player) ->
        move(InC, InL, DeC, DeL),
        (finish(Board) ->
            write('ganhaste parabens ueueueue')
        ;   generateRandomMove(NewBoard, Computer, ComInC, ComInL, ComDeC, ComDeL),
            move(ComInC, ComInL, ComDeC, ComDeL),
            finish(Board) ->
                write('loser')
            ;   vsHuman(Board, Player, Computer))
    ;   vsHuman(Board, Player, Computer)).

vsHuman(Board, Player, OtherPlayer) :-
    (analyseMove(Board, Player, HorMove, VertMove, PiecesToMove) ->
        move(Board, NewBoard, HorMove, VertMove, PiecesToMove, PiecesRemoved),
        finish(Player, OtherPlayer, Removed) ->
            displayGameOver
        ;   vsHuman(Board, OtherPlayer, Player)
    ;   vsHuman(Board, Player, OtherPlayer)).

game(Board) :-
    boardSize(Size),
    setupGame(Board, Size),
    vsHuman(Board, w-0, r-0).

oshi :-
    displayMenu,
    navigatingMenu(Choice),
    (Choice == 1 -> game(X), oshi
    ;Choice == 2 -> game(X), oshi
    ;Choice == 3 -> displayRules, oshi
    ;Choice == 4 -> write('Exiting Oshi. Hope you enjoyed yourself.\n\n')
    ;oshi).
