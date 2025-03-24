Data Module:

Central Data Management - managed database connections, data retrieval, processing, transformations, triggers, and refresh operations.

Functions:

-   `db_read_gmh_pre_lease_global_summary()`: 
    -   Reads the SQL view: `gmh.pre_lease_global_summary` which performs the data transformations and merging between the following tables:
        -   `entrata.report_pre_leaes` 
-   `db_read_gmh_pre_lease_property_summary()`
-   `db_read_gmh_pre_lease_property_details`