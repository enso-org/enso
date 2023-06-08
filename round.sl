with args as (select
  123.235 as n,
  2 as dp,
  false as use_bankers) select * from (
with v0 as (select
  power(10, args.dp) as scale from args) select * from (
with v1 as (select
  args.n * v0.scale as scaled from args, v0) select * from (
with v2 as (
  select floor(v1.scaled) as round_base from v1) select * from (
with v3 as (select
  (v2.round_base + 0.5) / v0.scale as round_midpoint from v2, v0) select * from (
with v4 as (select
  case when args.n >= 0
    then (((cast(trunc(v1.scaled) as integer)) % 2) != 0)
    else (((cast(trunc(v1.scaled) as integer)) % 2) = 0) end as even_is_up from args, v1) select * from (
with v5 as (select
  case when args.use_bankers then v4.even_is_up else true end as half_goes_up from args, v4) select * from (
with v6 as (select
  case when v5.half_goes_up then args.n >= v3.round_midpoint else n > v3.round_midpoint end as do_round_up from args, v3, v5) select * from (
select *, case when v6.do_round_up then (v2.round_base + 1.0) / v0.scale else v2.round_base / v0.scale end as result
from args, v0, v1, v2, v3, v4, v5, v6
) q7
) q6
) q5
) q4
) q3
) q2
) q1
) q0

;
