:-use_module(library(lists)).

board([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o]]).

boardStart([[piece(r, 3), o, o, o, o, o, o, o, piece(r, 3)],
    [o, o, piece(r, 2), piece(r, 1), piece(r, 1), piece(r, 1), piece(r, 2), o, o],
    [o, o, o, o, piece(r, 1), o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, piece(w, 1), o, o, o, o],
    [o, o, piece(w, 2), piece(w, 1), piece(w, 1), piece(w, 1), piece(w, 2), o, o],
    [piece(w, 3), o, o, o, o, o, o, o, piece(w, 3)]]).

boardMidGame([[o, o, o, o, o, o, o, o, o],
    [o, piece(r, 1), o, o, o, o, o, o, o],
    [o, o, o, o, piece(r, 1), o, o, o, o],
    [o, o, o, piece(r, 3), o, o, o, o, o],
    [o, o, o, o, o, o, piece(w, 2), o, o],
    [o, o, o, piece(w, 3), o, piece(r, 1), o, o, o],
    [o, o, piece(r, 3), o, o, o, o, o, o],
    [o, o, o, o, o, piece(w, 3), piece(w, 1), piece(r, 2), piece(r, 2)],
    [o, o, piece(w, 1), piece(w, 1), o, o, o, o, piece(w, 2)]]).

boardEndGame([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, piece(r, 1), o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, piece(w, 2), o, o],
    [o, o, o, o, o, piece(r, 1), o, o, o],
    [o, o, piece(r, 3), o, o, o, o, o, o],
    [o, o, o, o, o, piece(w, 3), o, piece(r, 2), piece(r, 2)],
    [o, o, piece(w, 1), piece(w, 1), o, o, o, o, o]]).

letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']).

translate(o) :- write(' ').
translate(piece(w, 3)) :- write('O').
translate(piece(w, 2)) :- write('o').
translate(piece(w, 1)) :- write('.').
translate(piece(r, 3)) :- write('X').
translate(piece(r, 2)) :- write('x').
translate(piece(r, 1)) :- write('*').

pieceHeight(piece(_, Height), Height).
pieceColor(piece(Color, _), Color).

convertLetterToIndex(Column, [X|Xs], Index, Result) :-
    Column \== X ->
        NewIndex is Index+1,
        convertLetterToIndex(Column, Xs, NewIndex, Result);
    returnResult(Index, Result).

setupGame(X, N) :-
    N < 10,
    N >= 0,
    boardStart(X),
    displayBoard(X, N, N).

midGame(X, N) :-
    boardMidGame(X),
    displayBoard(X, N, N).

end(X, N) :-
    boardEndGame(X),
    displayBoard(X, N, N).

displayCol(_, 0).
displayCol([X|Xs], I) :-
    Next is I-1,
    write('  '),
    write(X),
    write(' '),
    displayCol(Xs, Next).

displayLine(_, _, 0) :-
    write(' | '),
    nl.
displayLine([X|Xs], N, R) :-
    R1 is R-1,
    write(' | '),
    translate(X),
    displayLine(Xs, N, R1).

displaySeparator(_, 0) :- write('+').
displaySeparator(N, R) :-
    R1 is R-1,
    write('+---'),
    displaySeparator(N, R1).

displayBoard(_, N, 0) :-
    write('   '),
    displaySeparator(N, N),
    nl,
    write('   '),
    letters(A),
    displayCol(A, N).
displayBoard([L|Ls], N, R) :-
    R1 is R-1,
    write('   '),
    displaySeparator(N, N),
    nl,
    write(' '),
    write(R),
    displayLine(L, N, N),
    displayBoard(Ls, N, R1).

askMove(InC, InL, DeC, DeL) :-
    write('Coordinates of the piece to move (ex: \'d3\'.)'),
    nl,
    read(Input),
    atom_chars(Input, [InC|[InL|Rest]]),
    write('Coordinates of the pieces destination (ex: \'f3\'.)'),
    nl,
    read(Dest),
    atom_chars(Dest, [DeC|[DeL|Rest]]).

withinBoard(X, Y, Size) :-
    X > 0,
    X =< Size,
    Y > 0,
    Y =< Size.

isOrthogonal(InC, InL, DeC, DeL) :-
    (InC == DeC, InL \== DeL);
    (InC \== DeC, InL == DeL).

returnResult(Result, Result).

getListElement(Index, [X|Xs], Iterator, Result) :-
    Index =\= Iterator ->
        NewIterator is Iterator+1,
        getListElement(Index, Xs, NewIterator, Result);
    returnResult(X,Result).

isPiece(piece(_,_)).

getPiece(ColumnIndex, LineIndex, [X|Xs], Piece) :-
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

housesAffected([X|Xs], Column, Line, HorMove, VertMove, 0, Affected, Affected) :- write('ola').
housesAffected([X|Xs], Column, Line, HorMove, VertMove, Amount, Affected, Total) :-
    withinBoard(Column, Line, 9) ->
        (getPiece(Column, Line, [X|Xs],Piece) ->
            ((Affected - 1) >= 0 ->
                NewAffected is Affected - 1,
                NewAmount is Amount;
            !,fail)
        ;   NewAmount is Amount - 1,
            NewAffected is Affected),
        newHorizontalCoord(Column, HorMove, NewColumn),
        newVerticalCoord(Line, VertMove, NewLine),
        housesAffected([X|Xs], NewColumn, NewLine, HorMove, VertMove, NewAmount, NewAffected, Total)
    ;   Total is Affected.

/* Saves all pieces coordinates from one player still in the game in a list. Returns num remaining pieces too*/
getPiecesCoordinates(Board, Column, Line, Side, [PieceCoords|Locals], PiecesLeft, PiecesTotal) :-
    (Column + 1 > 9 ->
        NewCol is 1,
        NewLine is Line + 1
    ;   NewCol is Column + 1,
        NewLine is Line),

    (Line =< 9 ->
        (getPiece(Column, Line, Board, Piece) ->
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

a(A,B,Y,Z, Total) :-
    Max is 3,
    boardStart(X),
    housesAffected(X, A, B, Y, Z, 4, 4, InvTotal), write(InvTotal),
    Total is 4 - InvTotal.

moveLine([],_,_,_,_,_).
moveLine([X|Xs], [N|Ns], InC, DeC, CurrC, Piece) :-
    (   CurrC == InC -> N = o
    ;   CurrC == DeC -> N = Piece
    ;   N = X),
    NextC is CurrC + 1,
    moveLine(Xs, Ns, InC, DeC, NextC, Piece).

copyLine([], []).
copyLine([O|Os], [O|Ns]) :- copyLine(Os, Ns).

moveHorAuxiliar(_,_,_,_,0,_,_).
moveHorAuxiliar([X|Xs], [N|Ns], InC, InL, CurrLine, DeC, Piece) :-
    (CurrLine == InL ->
        moveLine(X, N, InC, DeC, 1, Piece)
    ;   copyLine(X, N)),
    NextLine is CurrLine - 1,
    moveHorAuxiliar(Xs, Ns, InC, InL, NextLine, DeC, Piece).

moveHorizontal([X|Xs], [N|Ns], InC, InL, DeC) :-
    getPiece(InC, InL, [X|Xs], Piece),
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
    ;   copyLine(X, N)),
    NextLine is CurrLine - 1,
    moveVerAuxiliar(Xs, Ns, InC, InL, NextLine, DeL, Piece).

moveVertical([X|Xs], [N|Ns], InC, InL, DeL) :-
    getPiece(InC, InL, [X|Xs], Piece),
    moveVerAuxiliar([X|Xs], [N|Ns], InC, InL, 9, DeL, Piece).

/* Missing Player and [X|Xs] and board size is currently hardcoded */
verifyMove(InC, InL, DeC, DeL) :-
    withinBoard(InC, InL, 9),
    withinBoard(DeC, DeL, 9),
    isOrthogonal(InC, InL, DeC, DeL).

%move([X|Xs], InC, InL, DeC, DeL).
%finish(X).

analyseMove([X|Xs], Player) :-
    askMove(InC, InL, DeC, DeL),
    letters(A),
    convertLetterToIndex(InC, A, 1, InColInd),
    convertLetterToIndex(DeC, A, 1, DeColInd),
    verifyMove(InColInd, InL, DeColInd, DeL).

%analyseMove([X|Xs], Player) :-
%    askMove([X|Xs], InC, InL, DeC, DeL),
%    verifyMove([X|Xs], InC, InL, DeC, DeL, P).

play(X, Player, OtherPlayer) :-
    analyseMove(X, Player),
    move(InC, InL, DeC, DeL),
    finish(X),
    play(X, OtherPlayer, Player).

game(X) :-
    setupGame(X, 9),
    play(X, a, b).
