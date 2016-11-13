:-use_module(library(lists)).
:-use_module(library(random)).
:-include('interface.pl').

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
        getListElement(Index, Xs, NewIterator, Result);
    returnResult(X,Result).

isPiece(piece(_,_)).

getPiece([X|Xs], ColumnIndex, LineIndex, Piece) :-
    getListElement(10-LineIndex, [X|Xs], 1, Line),
    getListElement(ColumnIndex, Line, 1, Piece),
    isPiece(Piece).

newHorizontalCoord(Column, HorMove, NewColumn) :-
    (HorMove > 0 -> NewColumn is Column+1);
    (HorMove < 0 -> NewColumn is Column-1);
    (HorMove == 0 ->NewColumn is Column).

newVerticalCoord(Line, VertMove, NewLine) :-
    (VertMove > 0 -> NewLine is Line+1);
    (VertMove < 0 -> NewLine is Line-1);
    (VertMove == 0 ->NewLine is Line).

housesAffected(_, _, _, _, _, 0, Affected, Affected, _).
housesAffected([X|Xs], Column, Line, HorMove, VertMove, Amount, Affected, Total, [Pieces|Rest]) :-
    withinBoard(Column, Line, 9) ->
        newHorizontalCoord(Column, HorMove, NewColumn),
        newVerticalCoord(Line, VertMove, NewLine),
        (getPiece([X|Xs], Column, Line, _) ->
            ((Affected - 1) >= 0 ->
                NewAffected is Affected - 1,
                NewAmount is Amount,
                returnResult(Pieces, Column-Line-Amount),
                housesAffected([X|Xs], NewColumn, NewLine, HorMove, VertMove, NewAmount, NewAffected, Total, Rest)
            ;!,fail)
        ;   NewAmount is Amount - 1,
            NewAffected is Affected,
            housesAffected([X|Xs], NewColumn, NewLine, HorMove, VertMove, NewAmount, NewAffected, Total, [Pieces|Rest]))
    ;   Total is Affected.

/* Saves all pieces coordinates from one player still in the game in a list. Returns num remaining pieces too*/
getPiecesCoordinates(Board, Column, Line, Side, [PieceCoords|Locals], PiecesLeft, PiecesTotal) :-
    (Column + 1 > 9 ->
        NewCol is 1,
        NewLine is Line + 1
    ;   NewCol is Column + 1,
        NewLine is Line),

    (Line =< 9 ->
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

deletePieces(_, _, [], Pieces, Pieces, 0).
deletePieces(X, CurrC, [Piece|Rest], Pieces, NewPieces, Deleted) :-
    getFirstValuePair(Piece, Column),
    CurrC == Column ->
        getSecondValuePair(Piece, Amount),
        DestC is Column + Amount,
        createDestPair(Pair, DestC, X),
        addToList(Pair, Pieces, NewPieces),
        Deleted is 1
    ;   deletePieces(X, CurrC, Rest, Pieces, NewPieces, Deleted).

addPieces(_, o, _, [], 1).
addPieces(X, X, _, [], 0).
addPieces(X, N, CurrC, [Piece|Rest], Deleted) :-
    getFirstValuePair(Piece, Column),
    CurrC == Column ->
        getSecondValuePair(Piece, Object),
        N = Object
    ;   addPieces(X, N, CurrC, Rest, Deleted).

moveLine([],_,_,_, Pieces, Pieces).
moveLine([X|Xs], [N|Ns], CurrC, PiecesToMove, Pieces, Removed) :-
    deletePieces(X, CurrC, PiecesToMove, Pieces, NewPieces, Deleted),
    addPieces(X, N, CurrC, NewPieces, Deleted),
    NextC is CurrC + 1,
    moveLine(Xs, Ns, NextC, PiecesToMove, NewPieces, Removed).

moveHorPositiveAux(_,_,_,_,0,_).
moveHorPositiveAux([X|Xs], [N|Ns], MovePairs, MoveLine, CurrLine, Removed) :-
    (CurrLine == MoveLine ->
        moveLine(X, N, 1, MovePairs, [], Removed)
    ;   returnResult(X, N)),
    NextLine is CurrLine - 1,
    moveHorPositiveAux(Xs, Ns, MovePairs, MoveLine, NextLine, Removed).

moveHorizontal([X|Xs], [N|Ns], MoveList, Direction, Removed) :-
    createMovementPairs(MoveList, Pairs),
    getMoveLine(MoveList, MoveLine),
    write(Pairs), nl, write(MoveLine), nl,
    (Direction > 0 ->
        moveHorPositiveAux([X|Xs], [N|Ns], Pairs, MoveLine, 9, MovedList)
    ;   nl),
    getOutsideBoard(MovedList, Outside), reverse(Outside, [_|Removed]).

test(NewLine, Removed, List) :-
    boardStart(X),
    moveList(Y),
    getListElement(2, X, 1, Line),
    createMovementPairs(Y, Pairs),
    ToAdd= [],
    moveLine(Line, NewLine, 1, Pairs, ToAdd, Removed),
    getOutsideBoard(Removed, List),
    displayLine(NewLine, 9, 9).

moveList([5-2-3, 6-2-3, 7-2-3]).

test2(Removed) :-
    boardStart(X),
    moveList(Y),
    moveHorizontal(X, N, Y, 1, Removed),
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
verifyMove(Board,InC, InL, DeC, DeL, Player, TotalAffected, PiecesAffected) :-
    withinBoard(InC, InL, 9),
    withinBoard(DeC, DeL, 9),
    isOrthogonal(InC, InL, DeC, DeL, HorMove, VertMove, Amount),
    getPiece(Board, InC, InL, Piece),
    pieceColor(Piece, Color),
    Color == Player,
    pieceHeight(Piece, Height),
    housesAffected(Board, InC, InL, HorMove, VertMove, Amount, Height + 1, InverseTotal, PiecesAffected),
    TotalAffected is Height + 1 -InverseTotal.

%move([X|Xs], InC, InL, DeC, DeL).
%finish(X).


listPossible(Column, Line, HorMove, VertMove, Amount, [Move|Rest]) :-
    Amount > 0 ->
        NewAmount is Amount-1,
        DestCol is Column + HorMove * Amount,
        DestLin is Line + VertMove * Amount,
        returnResult(Column-Line-DestCol-DestLin, Move),
        listPossible(Column, Line, HorMove, VertMove, NewAmount, Rest)
    ;   write('').

getPossibleDirection(_, _, _, _, _, 0, _, Total, Moves) :- Total is 0, returnResult(Moves, []).
getPossibleDirection(Board, Column, Line, HorMove, VertMove, Amount, Affected, Total, Moves) :-
    DestCol is Column + (HorMove * Amount),
    DestLin is Line + (VertMove * Amount),
    ((withinBoard(DestCol, DestLin, 9),
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
    random(0, NumMoves, Option),
    write(NumMoves+Option),nl,!,
    write(AllMoves),
    getListElement(Option, AllMoves, 0, InC-InL-DeC-DeL),
    write(InC+InL+DeC+DeL), nl.

analyseMove(Board, Player) :-
    askMove(InC, InL, DeC, DeL),
    letters(A),
    convertLetterToIndex(InC, A, 1, InColInd),
    convertLetterToIndex(DeC, A, 1, DeColInd),
    verifyMove(Board, InColInd, InL, DeColInd, DeL, Player, TotalAffected, PiecesAffected).

vsComputer(Board, Player, Computer).

vsHuman(Board, Player, OtherPlayer) :-
    (analyseMove(Board, Player) ->
        move(InC, InL, DeC, DeL),
        finish(Board) ->
            write('acabou')
        ;   vsHuman(Board, OtherPlayer, Player)
    ;   vsHuman(Board, OtherPlayer, Player)).

game(Board) :-
    setupGame(Board, 9),
    vsHuman(Board, w, r).

oshi :-
    displayMenu,
    navigatingMenu(Choice),
    (Choice == 1 -> game(X), oshi
    ;Choice == 2 -> game(X), oshi
    ;Choice == 3 -> displayRules, oshi
    ;Choice == 4 -> write('Exiting Oshi. Hope you enjoyed yourself.\n\n')
    ;oshi).
