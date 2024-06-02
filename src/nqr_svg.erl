-module(nqr_svg).
-export([tag/1]).

-include("qrcode.hrl").

tag(Data) when is_list(Data) ->
    tag(list_to_binary(Data));
tag(Data) when is_binary(Data) ->
    QrCode = nitro_qrcode:encode(Data),
    Svg = pixels(QrCode),
    Dim = QrCode#qrcode.dimension,
    #{
        viewbox=io_lib:format("0 0 ~p ~p", [Dim, Dim]),
        data=Svg
    }.

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

dimension(QrCode) ->
    QrCode#qrcode.dimension.
