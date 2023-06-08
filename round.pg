with n as (select 123.235 as n) select * from (
with dp as (select 2 as dp) select * from (
with use_bankers as (select false as use_bankers) select * from (
with scale as (select power(10, (select * from dp)) as scale) select * from (
with scaled as (select (select * from n) * (select * from scale) as scaled) select * from (
with round_base as (select floor((select * from scaled)) as round_base) select * from (
with round_midpoint as (select (((select * from round_base) + 0.5) / (select * from scale)) as round_midpoint) select * from (
with even_is_up as (select case when (select * from n) >= 0 then (select ((cast(trunc((select * from scaled)) as integer)) % 2) != 0) else (select ((cast(trunc((select * from scaled)) as integer)) % 2) = 0) end as even_is_up) select * from (
with half_goes_up as (select case when (select * from use_bankers) then (select * from even_is_up) else true end as half_goes_up) select * from (
with do_round_up as (select case when (select * from half_goes_up) then (select (select * from n) >= (select * from round_midpoint)) else (select (select * from n) > (select * from round_midpoint)) end as do_round_up) select * from (
with result as (select case when (select * from do_round_up) then (select ((select * from round_base) + 1.0) / (select * from scale)) else (select ((select * from round_base)) / (select * from scale)) end as result) select * from (

select n.*, dp.*, use_bankers.*, scale.*, scaled.*, round_base.*, round_midpoint.*, even_is_up.*, half_goes_up.*, do_round_up.*, result.*

from n, dp, use_bankers, scale, scaled, round_base, round_midpoint, even_is_up, half_goes_up, do_round_up, result

) q0
) q1
) q2
) q3
) q4
) q5
) q6
) q7
) q8
) q9
) q10

;
