# Economic Evaluation — Net Revenue and Net Present Value

## Definitions
Let:
- \\( P_c \\): crop price (USD/t)
- \\( P_l \\): lime price (USD/t)
- \\( Y_{resp} \\): yield response (t/ha)
- \\( L \\): lime rate (t/ha)
- \\( r \\): discount rate (decimal)
- \\( T \\): years
- \\( d \\): benefit decay (decimal)

## 1. Net Revenue
\\[
NR = Y_{resp} P_c - L P_l
\\]

## 2. Net Present Value
\\[
NPV = \sum_{t=1}^{T} \frac{(1-d)^{t-1} Y_{resp} P_c}{(1+r)^t} - L P_l
\\]

If \\( d = 0 \\):
\\[
NPV = Y_{resp} P_c \sum_{t=1}^{T} \frac{1}{(1+r)^t} - L P_l
\\]

## Interpretation
- \\( NPV > 0 \\): profitable investment.
- The peak of \\( NPV(L) \\) indicates the economically optimal lime rate.
