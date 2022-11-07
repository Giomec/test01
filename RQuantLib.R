install.packages("RQuantLib")
library(RQuantLib)
# https://www.r-project.org/conferences/useR-2010/slides/Eddelbuettel+Nguyen.pdf

# type: A string with one of the values call or put
# underlying: Current price of the underlying stock
# strike: Strike price of the option
# dividendYield: Continuous dividend yield (as a fraction) of the stock
# riskFreeRate: Risk-free rate
# maturity: Time to maturity (in fractional years)
# volatility:Volatility of the underlying stock
# timeSteps:Time steps for the “CrankNicolson” finite differences method engine, default value is 150
# gridPoints: Grid points for the “CrankNicolson” finite differences method, default value is 149
# engine: String selecting pricing engine, currently supported are “BaroneAdesiWhaley” and “CrankNicolson”
# discreteDividends: Vector of discrete dividends (optional)
# discreteDividendsTimeUntil:Vector of times to discrete dividends (in fractional years, optional)
# simple call with unnamed parameters 
AmericanOption("call", 100, 100, 0.02, 0.03, 0.5, 0.4)
# simple call with some explicit parameters
AmericanOption("put", strike=100, volatility=0.4, 100, 0.02, 0.03, 0.5)
# simple call with unnamed parameters, using Crank-Nicolons
AmericanOption("put", strike=100, volatility=0.4, 100, 0.02, 0.03, 0.5, engine="CrankNicolson")

AmericanOptionImpliedVolatility(type="call", value=120, underlying=100,
                                strike=100, dividendYield=0.25, riskFreeRate=0.08,
                                maturity=1, volatility=0.2)

# EuropeanOption("call", 100, 100, 0.01, 0.03, 0.5, 0.4)
EuropeanOption("call", 100, 100, 0.01, 0.03, 0.5, 0.4)
EuropeanOption("call", 100, 100, 0.12, 0.08, 0.25, 0.2)
AmericanOption("call", 100, 100, 0.12, 0.08, 0.25, 0.2)
