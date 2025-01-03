# Steps for manual testing of the app

 This document is to guide developers on how to manually test the app.
 Steps should be followed in order and be done in a single session from start to finish. 

1. Open app: https://bcgov-env.shinyapps.io/bc_wqg/
2. Click on User Guide tab
	a. Confirm formatting looks valid
3. Click on About tab
	a. Confirm formatting looks valid
	b. Check URLs are not broken 
4. Confirm footer links are valid
5. Click on WQG tab
	a. Click on download button
	b. Select 1,1-Dicholoroethylene
		i. Select Drinking water
			1) Confirm results appears and are formatted correctly  
			2) Check links are active and correct 
		ii. Tick water off and on
			1) Results should disappear if nothing ticked and then re-appear when ticked again
	c. Select Copper by typing copper 
		i. Select Aquatic Life - Marine, Aquatic Life - Freshwater and Wildlife
		ii. Confirm reports appear and are formatted correctly
			1) Check links are active and correct
		iii. Change Hardness value to 130
			1) No change
		iv. Change pH to 8
			1) Should cause a guideline to change
		v. Change Dissolved Organic Carbon to 2
			1) Should cause guidelines changes
		vi. Change Significant Figures to 2
			1) Should update values so only two significant figures are showing on calculated values
		vii. Change significant figures to 5
			1) Should increase values to multiple decimal points 
		viii. Tick off water media
			1) Parameter options should disappear 
	d. Select pH
		i. Add all values
		ii. Select a pH value of 1
      1) Results should appear
