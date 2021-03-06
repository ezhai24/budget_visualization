# Budget Visualization
This project was originally built for the UW Women's Rugby Club (UWWRC) to visualize a spreadsheet tracking club income and expenses. It can, however, be used with any type of data so long as it follows the format outlined below and is uploaded as a .csv file. **A sample data file is provided in the git repository.**

The project can be found here: (https://ezhai24.shinyapps.io/budget_visualization/)

## File Format
The uploaded .csv file must have the following 5 columns. Though they can be named whatever you like, they must be in this order from left to right.
* Date - The date the transaction occured in mm/dd/yyyy format.
* Description - A description of the transaction. Used for UWWRC records but not in app so can be left blank.
* Amount - The amount of money deposited or withdrawn in the transaction. If withdrawn, the number should be negative.
* Type - A type category associated with the transaction. Do not leave blank. (Use a stand-in like "--" or "other" for transactions without a type.)
* Subtype - A subtype category associated with the transaction. Do not leave blank. (Use a stand-in like "--" or "other" for transactions without a subtype.)

## UWWRC Types & Subtypes
* Administrative
  * USA Rugby Team Registration
  * Conference Dues
  * Treasurer supplies
* Fundraiser
  * Various fundraisers (ie. Apparel, Ruggerween)
* Bank (ie. checks, fees)
* Event
  * Various events (ie. Ruggerdays, Banquet)
* Equipment
  * Med kit
  * Nubbins
  * Warmup kit
  * Balls
* Coaches
  * Food
  * Apparel
  * CIPP
* Damages - Cleaning supplies/repair costs
* Game
  * Social
  * Gas
  * Vans
  * Parking
  * Trainer
* Dues
* Playoffs
