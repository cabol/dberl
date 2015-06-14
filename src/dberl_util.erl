%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Andres Bolaños, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2015, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc
%%% General purpose utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(dberl_util).

%% API
-export([keyfind/2, keyfind/3, proplist_to_map/1]).
-export([format_datetime/2, parse_datetime/1, datestr_to_ms/1, timestamp/1,
         now/0, now/1, timestamp_us/0, timestamp_ms/0, parse_timestamp_us/1,
         time_diff/1, time_diff/2]).
-export([bin_to_hex/1, hash_string/2, hmac/3, rand/1]).
-export([to_bin/1, to_atom/1, to_integer/1, to_float/1, to_list/1]).

%% Types
-type tuplist() :: [{any(), any()}].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Calls keyfind/3 with Default = undefined.
-spec keyfind(any(), tuplist()) -> any().
keyfind(Key, TupleList) ->
  keyfind(Key, TupleList, undefined).

%% @doc Searches the list of tuples TupleList for a tuple whose Nth element
%%      compares equal to Key. Returns Tuple's value if such a tuple is
%%      found, otherwise Default.
-spec keyfind(any(), tuplist(), any()) -> any().
keyfind(Key, TupleList, Default) ->
  case lists:keyfind(Key, 1, TupleList) of
    {_K, V} ->
      V;
    _ ->
      Default
  end.

%% @doc Transform a tuplist() into MAP.
-spec proplist_to_map(tuplist()) -> map().
proplist_to_map(TupleList) ->
  F = fun({K, V}, Acc) when is_list(V) ->
        maps:put(K, proplist_to_map(V), Acc);
        ({K, V}, Acc) ->
          maps:put(K, V, Acc)
      end,
  lists:foldl(F, #{}, TupleList).

%% @doc Format the given DateTime with the format specified in the atom().
-spec format_datetime(atom(), calendar:datetime()) -> string().
format_datetime(iso8601, DateTime) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
  lists:flatten(io_lib:format(
    "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
    [Year, Month, Day, Hour, Min, Sec]));
format_datetime(yyyymmdd, DateTime) ->
  {{Year, Month, Day}, _} = DateTime,
  lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B", [Year, Month, Day]));
format_datetime(rfc1123, DateTime) ->
  LocalTime = calendar:universal_time_to_local_time(DateTime),
  httpd_util:rfc1123_date(LocalTime).

%% @doc Converts a date string into datetime() format.
-spec parse_datetime(iolist()) -> calendar:datetime().
parse_datetime(DateStr) ->
  httpd_util:convert_request_date(to_list(DateStr)).

%% @doc Converts a date string to milliseconds timestamp.
-spec datestr_to_ms(string()) -> non_neg_integer().
datestr_to_ms(DateStr) ->
  timestamp(parse_datetime(DateStr)).

%% @doc Converts a date to milliseconds timestamp.
-spec timestamp(calendar:datetime()) -> non_neg_integer().
timestamp(Date) ->
  (calendar:datetime_to_gregorian_seconds(Date) - 62167219200) * 1000.

%% @doc Return current datetime in RFC-1123 format.
-spec now() -> binary().
now() ->
  iolist_to_binary(httpd_util:rfc1123_date(calendar:local_time())).

%% @doc Return current datetime in the given format.
-spec now(atom()) -> binary().
now(iso8601) ->
  iolist_to_binary(format_datetime(iso8601, calendar:universal_time()));
now(yyyymmdd) ->
  iolist_to_binary(format_datetime(yyyymmdd, calendar:universal_time()));
now(_) ->
  iolist_to_binary(httpd_util:rfc1123_date(calendar:local_time())).

%% @doc Returns a timestamp in microseconds.
-spec timestamp_us() -> non_neg_integer().
timestamp_us() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000000 + Micro.

%% @doc Returns a timestamp in milliseconds.
-spec timestamp_ms() -> non_neg_integer().
timestamp_ms() ->
  {Mega, Sec, _Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000.

%% @doc Parse a given microseconds timestamp to os:timestamp() format.
-spec parse_timestamp_us(integer()) -> {integer(), integer(), integer()}.
parse_timestamp_us(Timestamp) ->
  {Timestamp div 1000000000000,
   Timestamp div 1000000 rem 1000000,
   Timestamp rem 1000000}.

%% @doc Return the time difference between the given date time and the
%%      current one.
-spec time_diff(iolist()) -> integer().
time_diff(DateTime) ->
  T1 = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
  T2 = calendar:datetime_to_gregorian_seconds(parse_datetime(DateTime)),
  T2 - T1.

%% @doc Return the time difference between the given 2 dates.
-spec time_diff(iolist(), iolist()) -> integer().
time_diff(DateTime1, DateTime2) ->
  T1 = calendar:datetime_to_gregorian_seconds(parse_datetime(DateTime1)),
  T2 = calendar:datetime_to_gregorian_seconds(parse_datetime(DateTime2)),
  T2 - T1.

%% @doc Converts the given binary to hex string.
%% @see [hd(integer_to_list(Nibble, 16)) || <<Nibble:4>> <= B]
-spec bin_to_hex(binary()) -> string().
bin_to_hex(B) when is_binary(B) ->
  bin_to_hex(B, []).

%% @doc Hash the given data and return the hex-string result.
-spec hash_string(atom(), iodata()) -> string().
hash_string(Hash, Data) ->
  bin_to_hex(crypto:hash(Hash, Data)).

%% @doc Wrap original hmac/3 from erlang crypto module, and converts it
%%      result into hex string to return it.
%% @see Erlang crypto:hmac(Type, Key, Data).
-spec hmac(atom(), iodata(), iodata()) -> string().
hmac(Type, Key, Data) ->
  bin_to_hex(crypto:hmac(Type, Key, Data)).

%% @doc Generates N bytes randomly uniform 0..255, and returns result encoded
%%      Base64. Uses a cryptographically secure prng seeded and periodically
%%      mixed with operating system provided entropy. By default this is the
%%      RAND_bytes method from OpenSSL.
%%      May throw exception low_entropy in case the random generator failed
%%      due to lack of secure "randomness".
-spec rand(integer()) -> binary().
rand(N) ->
  F = fun($/) -> $X; ($+) -> $Y; (D) -> D end,
  <<<<(F(D))>> || <<D>> <= base64:encode(crypto:strong_rand_bytes(N)), D =/= $=>>.

%% @doc Converts any type to binary.
-spec to_bin(any()) -> binary().
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  float_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data);
to_bin(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_binary(erlang:phash2(Data));
to_bin(Data) ->
  Data.

%% @doc Converts any type to atom.
-spec to_atom(any()) -> atom().
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

%% @doc Converts any type to integer.
-spec to_integer(any()) -> integer().
to_integer(Data) when is_binary(Data) ->
  binary_to_integer(Data);
to_integer(Data) when is_list(Data) ->
  list_to_integer(Data);
to_integer(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_integer(Data) ->
  Data.

%% @doc Converts any type to float.
-spec to_float(any()) -> float().
to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_list(Data) ->
  list_to_float(Data);
to_float(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_float(Data) ->
  Data.

%% @doc Converts any type to list.
-spec to_list(any()) -> list().
to_list(Data) when is_binary(Data) ->
  binary_to_list(Data);
to_list(Data) when is_integer(Data) ->
  integer_to_list(Data);
to_list(Data) when is_float(Data) ->
  float_to_list(Data);
to_list(Data) when is_atom(Data) ->
  atom_to_list(Data);
to_list(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_list(erlang:phash2(Data));
to_list(Data) ->
  Data.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
hexdigit(C) when C >= 0, C =< 9 ->
  C + $0;
hexdigit(C) when C =< 15 ->
  C + $a - 10.

%% @private
bin_to_hex(<<>>, Acc) ->
  lists:reverse(Acc);
bin_to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
  bin_to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).
