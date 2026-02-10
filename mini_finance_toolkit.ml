(* Simple Interest Calculator *)
let simple_interest principal rate time =
  (principal *. rate *. time) /. 100.0

(* Total amount with Simple Interest *)
let simple_interest_amount principal rate time =
  principal +. simple_interest principal rate time


(* Total amount with Compound Interest *)
let compound_interest principal rate time =
  principal *. ((1.0 +. rate /. 100.0) ** time)

(* Compound Interest Calculator *)
let compound_interest_amount principal rate time =
  compound_interest principal rate time -. principal


(* SIP Calculator Simplified Formula - amount * months = I -> I * (1.0 +. annual_rate /. 100.0) *)
let sip_total_investment monthly_amount months =
  monthly_amount *. float_of_int months

let sip_estimated_value monthly_amount months annual_rate =
  let invested = sip_total_investment monthly_amount months in
  invested *. (1.0 +. annual_rate /. 100.0)

let sip_profit monthly_amount months annual_rate =
  sip_estimated_value monthly_amount months annual_rate
  -. sip_total_investment monthly_amount months


(* Net Worth Calculator *)
let total list =
  List.fold_left ( +. ) 0.0 list

let net_worth assets liabilities =
  total assets -. total liabilities (* total value of assets - total value of liabilities *)


(* 
   In utop - #use "mini_finance.ml";;
   
   Simple Interest - simple_interest P.0 R.0 T.0;;
                     simple_interest_amount P.0 R.0 T.0;;
   
   Compound Interest - compound_interest P.0 R.0 T.0;;
                       compound_interest_amount P.0 R.0 T.0;;
   
   SIP - sip_estimated_value Installments.0 Months Rate.0;;
         sip_profit Installments.0 Months.0 Rate.0;;

   Net Worth - net_worth [Total value of assets] [Total value of liabilities];;      
*)