<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# shinywqg 0.1.4 (2026-11-24)

- Added support for lookup tables that contain multiple substances, such as the chlorophenols lookup table (#102).
- Input boxes are now created for all EMS codes in the codes table rather than only those referenced in the Limit and Condition columns (#103).
- Added app version to About page (#103)

# shinywqg 0.1.3 (2026-03-06)

- Update internal data to include the current set of lookup tables (#100). 
- Fixed bug in evaluating formulas where they were being evaluated alphabetically instead of numerically (#99).
- Launch speed of app increased because only limits table is downloaded at the launch of the app (#98).

# shinywqg 0.1.2 (2025-04-02)

- Added total mercury (EMS_HG_T) to codes list to fix bug in Mercury look up.

# shinywqg 0.1.1 (2025-01-20)

- Updated internal data.
- Changed Salinity unites from ng/L to ppt (parts per thousand) in the user selection box. This was a labeling change for the variable and has no effect on the guideline generated. 
- Added minimum version requirement for the `chromote` package due to issue with random port connection. 

# shinywqg 0.1.0.9000 (2024-11-26)

- Package maintenance.  
