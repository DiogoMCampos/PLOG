:-use_module(library(lists)).
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
                getPiecesCoordinates(Board, NewCol, NewLine, Side, Locals, NewPiecesLeft, PiecesTotal),!
            ;   getPiecesCoordinates(Board, NewCol, NewLine, Side, [PieceCoords|Locals], PiecesLeft, PiecesTotal),!)
        ;   getPiecesCoordinates(Board, NewCol, NewLine, Side, [PieceCoords|Locals], PiecesLeft, PiecesTotal),!)
    ;   returnResult(PiecesLeft, PiecesTotal)).

abc(X) :- boardMidGame(Board), getPiecesCoordinates(Board, 1,1,X,Coords, 0, Pieces), write(Coords),nl,write(Pieces).

a(A,B,Y,Z) :-
    boardStart(X),
    housesAffected(X, A, B, Y, Z, 3, 4, InvTotal, [_|Rest]), reverse(Rest, [_|Real]),write(Real),
    Total is 4 - InvTotal,
    nl, write(Total).

moveLine([],_,_,_,_,_).
moveLine([X|Xs], [N|Ns], InC, DeC, CurrC, Piece) :-
    (   CurrC == InC -> N = o
    ;   CurrC == DeC -> N = Piece
    ;   N = X),
    NextC is CurrC + 1,
    moveLine(Xs, Ns, InC, DeC, NextC, Piece).

%copyLine([], []).
%copyLine([O|Os], [O|Ns]) :- copyLine(Os, Ns).

moveHorAuxiliar(_,_,_,_,0,_,_).
moveHorAuxiliar([X|Xs], [N|Ns], InC, InL, CurrLine, DeC, Piece) :-
    (CurrLine == InL ->
        moveLine(X, N, InC, DeC, 1, Piece)
    ;   returnResult(X, N)),
    NextLine is CurrLine - 1,
    moveHorAuxiliar(Xs, Ns, InC, InL, NextLine, DeC, Piece).

moveHorizontal([X|Xs], [N|Ns], InC, InL, DeC) :-
    getPiece([X|Xs], InC, InL, Piece),
    moveHorAuxiliar([X|Xs], [N|Ns], InC, InL, 9, DeC, Piece).

removeFromLine([],_,_,_).
removeFromLine([X|Xs], [N|Ns], InC, CurrC) :-
    (   CurrC == InC -> N = o
    ;   N = X),
    NextC is CurrC + 1,
    removeFromLine(Xs, Ns, InC, NextC).

addToLine([],_,_,_,_).
addToLine([X|Xs], [N|Ns], DeC, CurrC, Piece) :-
    (   CurrC == DeC -> N = Piece
    ;   N = X),
    NextC is CurrC + 1,
    addToLine(Xs, Ns, DeC, NextC, Piece).

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

analyseMove(Board, Player) :-
    askMove(InC, InL, DeC, DeL),
    letters(A),
    convertLetterToIndex(InC, A, 1, InColInd),
    convertLetterToIndex(DeC, A, 1, DeColInd),
    verifyMove(Board, InColInd, InL, DeColInd, DeL, Player, TotalAffected, PiecesAffected).

%analyseMove([X|Xs], Player) :-
%    askMove([X|Xs], InC, InL, DeC, DeL),
%    verifyMove([X|Xs], InC, InL, DeC, DeL, P).

play(X, Player, OtherPlayer) :-
    (analyseMove(X, Player) ->
        move(InC, InL, DeC, DeL),
        finish(X) ->
            write('acabou')
        ;   play(X, OtherPlayer, Player)
    ;   play(X, OtherPlayer, Player)).

game(X) :-
    setupGame(X, 9),
    play(X, w, r).

oshi :-
    displayMenu,
    navigatingMenu(Choice),
    (Choice == 1 -> game(X), oshi
    ;Choice == 2 -> game(X), oshi
    ;Choice == 3 -> displayRules, oshi
    ;Choice == 4 -> write('Exiting Oshi. Hope you enjoyed yourself.\n\n')
    ;oshi).
