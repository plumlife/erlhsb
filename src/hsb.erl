-module(hsb).

-export([ from_rgb/1 ]).

-include_lib("hsb.hrl").

-spec from_rgb(#rgb{}) -> #hsb{}.
from_rgb(#rgb{ red=R, green=G, blue=B }) ->
    
    %% Initialize RGB
    Red   = R / 255,
    Green = G / 255,
    Blue  = B / 255,
    
    Min = lists:min([Red, Green, Blue]),
    Max = lists:max([Red, Green, Blue]),
    Delta = Max - Min,
    
    B = Max,

    {H, S} = rgb_to_hsb({Max, Delta}, {Red, Green, Blue}),
    #hsb{ hue=H, saturation=S, brightness=B}.

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

increment_hue(H)
  when H < 0 ->
    H + 1;
increment_hue(H)
  when H > 1 ->
    H - 1.
