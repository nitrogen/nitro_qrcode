-module(qrcode_svg).
-export([tag/1]).

% Include necessary library
-include("qrcode.hrl").

tag(Data) when is_binary(Data) ->
    QrCode = qrcode:encode(Data),
    Svg = pixels(QrCode),
    SvgTag = io_lib:format("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 ~p ~p'>~s</svg>", [width(QrCode), height(QrCode), Svg]),
    iolist_to_binary(SvgHtml).

pixels(#qrcode{data=Data, dimension=Dim}) ->
    pixels(Data, Dim).

pixels(Data, Dim) ->
    Rows = peel_row_bits(Data, Dim, 0),
    process_rows(Rows).

peel_row_bits(<<>>, _, _) ->
    [];
peel_row_bits(Data, Dim, RowNum) ->
    <<RowBits:Dim/bits, Bits/bits>> = Data,
    [{RowNum, RowBits} | peel_row_bits(Bits, Dim, RowNum+1)].

process_rows(Rows) ->
    lists:map(fun({RowNum, Row}) ->
        process_bits(RowNum, 0, Row)
    end, Rows).
   
process_bits(_, _, <<>>) ->
    [];
process_bits(RowNum, ColNum, <<1:1, Bits/bits>>) ->
    [black_pixel(RowNum, ColNum) | process_bits(RowNum, ColNum+1, Bits)];
process_bits(RowNum, ColNum, <<0:1, Bits/bits>>) ->
    %% white pixel - no export;
    process_bits(RowNum, ColNum+1, Bits).

black_pixel(RowNum, ColNum) ->
    io_lib:format("<rect x='~p' y='~p' width='1' height='1' fill='black' />", [ColNum, RowNum]).

width(QrCode) ->
    QrCode#qrcode.dimension.

height(QrCode) ->
    QrCode#qrcode.dimension.
