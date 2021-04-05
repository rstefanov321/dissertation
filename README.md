# My Dissertation
<h2> Brief Overview / Title of the dissertation </h2>

An analysis of <b>Cryptocurrencies' potential to become a wealth-preservation alternative to precious metals during capital market volatility.</b>

The asymmetric GJR-GARCH statistical model used to show the price behaviour differences/similarities between cryptos, precious metals and macroeconomic factors' prices. 

<h2> Outcome of the research </h2>

The results show that the chosen cryptocurrencies have a completely different price development from gold and the remaining assets under analysis. Consequently, their inclusion in a portfolio could potentially lead to its better diversification. However, with higher returns come higher risks, therefore, investing into cryptos should be preceded by a careful considerations of these risks. 

<h2> Data collected and used </h2>

<h3> Cryptocurrencies' data </h3>

My dissertation was written in R and I used historical daily closing prices taken at midnight, GMT (collected from Yahoo Finance) of the following cryptocurrencies:
 <ul>
   <li>Bitcoin (BTC / USD);</li>
   <li>Ripple (XRP / USD);</li>
   <li>Litecoin (LTC / USD);</li>
   <li>Ethereum (ETH / USD)</li>
</ul>

<h3> Here are the close-price charts for each of the cryptos and their returns per day. </h3>

![CL_BTC](https://user-images.githubusercontent.com/81915517/113561213-e626ab80-960c-11eb-99f2-2515302fc0b5.png)
![R_F_BTC](https://user-images.githubusercontent.com/81915517/113561556-71a03c80-960d-11eb-8119-a1996a3c9333.png)

![CL_XRP](https://user-images.githubusercontent.com/81915517/113561069-b4ade000-960c-11eb-86f9-ddfc5cc70d2f.png)
![R_F_XRP](https://user-images.githubusercontent.com/81915517/113561605-854ba300-960d-11eb-9934-8013ba2d8037.png)

![CL_LTC](https://user-images.githubusercontent.com/81915517/113561115-c2636580-960c-11eb-8d9e-79ed490261ec.png)
![R_F_LTC](https://user-images.githubusercontent.com/81915517/113561618-8b418400-960d-11eb-9683-f94719f200ea.png)

![CL_ETH](https://user-images.githubusercontent.com/81915517/113561128-c7c0b000-960c-11eb-90ce-5e7c4683def2.png)
![R_F_ETH](https://user-images.githubusercontent.com/81915517/113561626-8ed50b00-960d-11eb-8667-75ed83ed2fce.png)


The time series data for Bitcoin, Litecoin and Ripple spans from 17th of September 2014 until the 27th of November 2019, amounting to 1898 days.
<b>The only exception is Ethereum</b> (ICO was in mid 2015). Consequently, the data observed starts from the 7th of August 2015 and comprises 1574 days.

<h3> Macroeconomic factors’ data </h3>

For further details on the sources and their charts, see my dissertaion.  

-	Euro - US dollar (EUR/USD),
-	British pound - US dollar (GBP/USD) and 
-	US dollar – Swiss franc (USD/CHF) daily exchange rates;
-	The COMEX gold futures rates (XAU/USD) for a troy ounce; 
-	The Overnight rates of the London Interbank Offered Rate (LIBOR).
-	Silver futures (XAG/USD) for a troy ounce denominated in USD; 
-	The S&P 500 Index;
-	The DAX 30 Index and
-	The US dollar Trade-weighted index, used as external regressor.

<h4> Data preprocessing </h4>

As Cryptos are traded 24/7, the other investment tools aren't , therefore, <b>interpolation </b> was used to fill the missing values for Saturdays, Sundays or bank holidays. 



<h2> Limitations to the research </h2> 

- results apply only for the given time period and future snapshots might reveal a different picture for the investor;
- a better representation could be done when a larger number of cryptos is compared.




