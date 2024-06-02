% This module is Copyright 2024 Jesse Gumm <gumm@sigma-star.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.


-module(nqr_svg).
-export([tag/1]).

-include("qrcode.hrl").

tag(Data) when is_list(Data) ->
    tag(list_to_binary(Data));
tag(Data) when is_binary(Data) ->
    QrCode = nitro_qrcode:encode(Data),
    Dim = QrCode#qrcode.dimension,
    Contents = iolist_to_binary(pixels(QrCode)),
    Viewbox = iolist_to_binary(io_lib:format("0 0 ~p ~p", [Dim, Dim])),
    Attrs = [{viewbox, Viewbox}],
    {Attrs, Contents}.

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

process_rows(Rows)  ->
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
