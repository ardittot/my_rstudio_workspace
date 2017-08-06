setwd("~/workspace/display_opt/MTA130/")

"
xts_afb : Android FB
xts_ifb : Android FB
xts_abbm : Android BBM
xts_ibbm : iOS BBM
xts_auac : Android UAC
xts_iuac : iOS UAC
xts_amobi : Android InMobi
xts_imobi : iOS InMobi # filter out all small cost daily data < 10
xts_aglispa : Android glispa
xts_imundo : iOS Mundomedia # weekly agg
xts_asource : Android IronSource # filter out all small cost daily data < 10


PS: 
(-) July data is on monthly (not daily) so need to adjust with Mundo channel
(-) cost_pred_channel <- fcast_<channelname>$mean[[1]]
(-) Channel name orders in nloptr output:
    1. Android BBM
    2. Android FB
    3. Android glispa
    4. Android inMobi
    5. Android IronSource
    6. Android UAC
    7. iOS BBM
    8. iOS FB
    9. iOS InMobi
    10.iOS Mundomedia
    11.iOS UAC
"
  