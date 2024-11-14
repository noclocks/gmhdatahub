# **Data Pipeline for the Leasing Summary Table**

---

## **1. Retrieve Properties**
   - **Step**: Connect to the Entrata API `/properties` endpoint with authentication.
   - **Result**: Retrieves property data with identifiers (`PropertyID`, `MarketingName`).
   - **Purpose**: Provides a list of property IDs to filter data in subsequent report requests.

---

## **2. Request Leasing Data Summary Report Data**
   - **Step**: Connect to `/reports` endpoint using `getReportData` method for the `pre_lease` report.
   - **Purpose**: Gather core preleasing data for properties.
   - **Request Body Parameters**:
      - **`reportName`**: `"pre_lease"` (indicates preleasing data).
      - **`reportVersion`**: `3.2` (compatibility with required structure).
      - **`filters`**: Limits data to relevant properties and date.
         - **`property_group_ids`**: List of `PropertyID`s.
         - **`period`**: Specifies date and period type, e.g., `'09/01/YYYY'` with `period_type` set to `'date'`.
         - **`summarize_by` and `group_by`**: `'property'` for property-level aggregation.
         - **`consider_pre_leased_on`**: e.g., `"332"` for specific preleasing status.
      - **Additional Filters**: Options such as `subtotals`, year-over-year (`yoy`), and occupancy or space options to control data aggregation, subtotaling, or calculations.

### **Queue ID Retrieval**
   - **Outcome**: Upon submission, the `getReportData` request returns a `queueId` token.
   - **Purpose**: Tracks and retrieves the report data asynchronously.

---

## **3. Retrieve Weekly Prelease Data**
   - **Objective**: Gather recent leasing activity, such as new leases and renewals, for each property.
   - **Step**: Use `/reports` endpoint with `getReportData` method to request the `"lease_execution_(applicant)"` report.
   - **Request Body Parameters**:
      - **`reportName`**: `"lease_execution_(applicant)"`, indicating weekly prelease data.
      - **`reportVersion`**: `1.8`.
      - **`filters`**:
         - **`property_group_ids`**: List of `PropertyID`s for relevant properties.
         - **`period`**: Defined as a `daterange` to gather data over the past seven days.
            - **`daterange-start`**: Calculated as `today - 7 days`.
            - **`daterange-end`**: Set to the current date.
         - **`results_based_on`**: `"activity"`, indicating that the data reflects leasing activity within the specified period.
         - **`lease_type`**: Includes new leases and renewals (e.g., `1` for new leases and `3` for renewals).
         - **`summarize_by` and `group_by`**: Set to `"lease_type"` and `"property"` respectively, grouping results by lease type and property.

   - **Queue ID Retrieval**:
      - **Outcome**: This request returns a unique `queueId`, allowing for asynchronous data retrieval.
      - **Purpose**: Track and manage the report generation progress.

### **Process and Aggregate Weekly Prelease Data**
   - After retrieving the report using `getResponse` on the `/queue` endpoint (via the `queueId`), the weekly prelease data is parsed and structured.
   - **Fields Extracted**:
      - `property_name`: Property name.
      - `lease_type`: Type of lease (e.g., "New Lease" or "Renewal").
      - `signed`: Count of signed leases.
   - **Data Transformation**:
      - Restructure data to aggregate new leases and renewals into columns like `new_leases` and `renewals`, replacing missing values with `0`.
      - Weekly prelease data is now in a format suitable for integration with the main leasing summary table, providing insights into leasing velocity and recent leasing activity.

---

## **4. Retrieve Completed Summary Report Data**
   - **Step**: Use the `queueId` with `/queue` endpoint to call `getResponse`.
   - **Request Body Parameters**:
      - **`queueId`**: Token from the `getReportData` request for the `pre_lease` report.
      - **`serviceName`**: `"getReportData"`, identifying the report request.

### **Retry Logic**
   - **Purpose**: Ensures data availability by retrying requests until the report is ready.

---

## **5. Parse and Structure the Final Report Data**
   - **Outcome**: Successfully retrieved JSON data from `getResponse` contains structured preleasing information.
   - **Data Processing**:
      - **Calculations**:
         - **Occupancy Rate**: Calculated as the ratio of occupied units to total units.
         - **Prelease Percentage**: Ratio of units preleased to total available units.
         - **Year-over-Year Variance**: Difference between current and previous year leasing activity.
         - **Leasing Velocity**: Number of units needed per week to meet overall goals.
      - **Weekly Prelease Integration**: The parsed weekly prelease data is joined with the main report data, enriching it with real-time leasing activity metrics.
      - **Manual "Model Beds" Column**:
         - **Purpose**: The "Model Beds" column, which indicates the number of units designated as model units (not for leasing), is manually managed outside of the Entrata API.
         - This data is retrieved from a local database table and merged with the main summary data for each property, allowing adjustments as needed based on updated model bed counts.
      - **"Investment Partners" Data**:
         - **Purpose**: The summary table includes a column to display investment partners for each property.
         - Investment partners are managed separately from the leasing data and stored in a dedicated database table. The application merges this data with the summary table, ensuring each property’s leasing data is paired with its respective investment partner.

   - **Purpose**: Prepares a detailed summary for each property, with calculated metrics for both the current leasing season and recent weekly activity.

---

## **6. Store and Use Summary Data**

### **Data Persistence**
   - **Step**: Store the processed data in a database to ensure consistent access and reduce repeated API calls.
   - **Purpose**: Database storage enables reliable and efficient access to data, including external metrics like "Model Beds" and "Investment Partners".

### **Data Freshness Check**
   - **Purpose**: Checks if the data is up-to-date by comparing the last update timestamp.
   - **Action**: If outdated, a new API request is triggered to refresh the data.

---

## **7. Display the Summary Table**

   - **Outcome**: The prepared summary data, including the integrated weekly prelease data, "Model Beds," and "Investment Partners," is displayed in the user interface.
   - **Features**:
      - Users can filter by specific properties.
      - The summary table dynamically updates, showing up-to-date leasing performance metrics, recent weekly prelease trends, and external metrics like model beds and investment partners for each property.
   - **Purpose**: Provides property managers with an actionable and comprehensive view of leasing performance, recent activity, and investment context.

---

This pipeline combines Entrata API data, manually managed metrics, and supplementary investment information, offering property managers a detailed, up-to-date view of leasing activity, trends, and property-specific details.