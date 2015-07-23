-module(hsb).

-export([ rgb2hsb/1, hsb2rgb/1 ]).

-include("hsb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% RGB to HSB functions
%% ===================================================================

-spec rgb2hsb(#rgb{}) -> #hsb{}.
rgb2hsb(#rgb{ red=R, green=G, blue=B }) ->
    
    %% Initialize RGB
    Red   = R / 255,
    Green = G / 255,
    Blue  = B / 255,
    
    Min = lists:min([Red, Green, Blue]),
    Max = lists:max([Red, Green, Blue]),
    Delta = Max - Min,
    
    Brightness = Max,

    {H, S} = rgb_to_hsb({Max, Delta}, {Red, Green, Blue}),
    #hsb{ hue=H, saturation=S, brightness=Brightness}.

-spec rgb_to_hsb({ Max::integer()
                 , Del::integer()
                 },
                 { Red::integer()
                 , Green::integer()
                 , Blue::integer()
                 }) -> {Huge::integer(), Saturation::integer()}.
rgb_to_hsb({_, 0}, _) ->
    {0, 0};
rgb_to_hsb({Max, Delta}, {Red, Green, Blue}) ->
    S = Delta / Max,
    
    DeltaRed   = ( ( ( Max - Red   ) / 6 ) + ( Delta / 2 ) ) / Delta,
    DeltaGreen = ( ( ( Max - Green ) / 6 ) + ( Delta / 2 ) ) / Delta,
    DeltaBlue  = ( ( ( Max - Blue  ) / 6 ) + ( Delta / 2 ) ) / Delta,

    H = rgb_delta_to_hsb(Max, {DeltaRed, DeltaGreen, DeltaBlue}, {Red, Green, Blue}),

    {H, S}.

rgb_delta_to_hsb(Max, {_, DeltaGreen, DeltaBlue}, {Red, _, _})
  when Red == Max ->
    H = DeltaBlue - DeltaGreen,
    increment_hue(H);

rgb_delta_to_hsb(Max, {DeltaRed, _, DeltaBlue}, {_, Green, _})
  when Green == Max ->
    H = ( 1 / 3 ) + DeltaRed - DeltaBlue,
    increment_hue(H);

rgb_delta_to_hsb(Max, {DeltaRed, DeltaGreen, _}, {_, _, Blue})
  when Blue == Max ->
    H = ( 2 / 3 ) + DeltaGreen - DeltaRed,
    increment_hue(H).

increment_hue(H) ->
    if
        H < 0.0 ->
            H + 1;
        H > 1.0 ->
            H - 1;
        true ->
            H
    end.

%% ===================================================================
%% HSB to RGB functions
%% ===================================================================
-spec hsb2rgb(#hsb{}) -> #rgb{}.
hsb2rgb(#hsb{ hue=_, saturation=0, brightness=Brightness }) ->
    
    %% Initialize RGB
    Red   = Brightness * 255,
    Green = Brightness * 255,
    Blue  = Brightness * 255,
    #rgb{ red=Red, green=Green, blue=Blue };

hsb2rgb(#hsb{ hue=Hue, saturation=Saturation, brightness=Brightness }) ->
    H = case (Hue * 6) of
            6 -> 0;
            V -> V
        end,

    I = trunc(H),
    V1 = Brightness * ( 1 - Saturation ),
    V2 = Brightness * ( 1 - Saturation * ( H - I ) ),
    V3 = Brightness * ( 1 - Saturation * ( 1 - ( H - I ) ) ),

    {R, G, B} = build_hsb(I, {V1, V2, V3}, {Hue, Saturation, Brightness}),
    #rgb{ red=round(R*255), green=round(G*255), blue=round(B*255)}.
    
build_hsb(0, {V1, _, V3}, {_, _, V}) ->
    {V, V3, V1};
build_hsb(1, {V1, V2, _}, {_, _, V}) ->
    {V2, V, V1};
build_hsb(2, {V1, _, V3}, {_, _, V}) ->
    {V1, V, V3};
build_hsb(3, {V1, V2, _}, {_, _, V}) ->
    {V1, V2, V};
build_hsb(4, {V1, _, V3}, {_, _, V}) ->
    {V3, V1, V};
build_hsb(_, {V1, V2, _}, {_, _, V}) ->
    {V, V1, V2}.

-ifdef(TEST).

rgb_to_hsb_test() ->
    RGB1 = #rgb{ red=255, green=14, blue=20 },
    RGB2 = hsb2rgb(rgb2hsb(RGB1)),
    ?assert(RGB1 == RGB2).

-endif.
