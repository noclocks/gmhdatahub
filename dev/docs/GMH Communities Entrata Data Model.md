# GMH Communities Entrata Data Model



**Entity Definitions:**

1. **Portfolios**
   - **Attributes**: PortfolioID, Name, Description, Owner, CreatedDate, UpdatedDate
   - **Relationships**: Links to Properties via PortfolioAssignments

2. **PortfolioAssignments**
   - **Attributes**: AssignmentID, PortfolioID, PropertyID, AssignmentDate, IsActive
   - **Relationships**: Links Properties to Portfolios

3. **Properties**
   - **Attributes**: PropertyID, Name, Type, Status, CreatedDate, UpdatedDate
   - **Relationships**: Linked to Units, FloorPlans, Buildings, Leasing Data, and PortfolioAssignments

4. **Property Units**
   - **Attributes**: UnitID, PropertyID, UnitTypeID, FloorPlanID, Status, Number, Floor, SquareFeet
   - **Relationships**: Linked to Leases, SpaceOptions, LeaseCharges, and Residents

5. **Buildings**
   - **Attributes**: BuildingID, PropertyID, Name, Floors, Status, Description
   - **Relationships**: Linked to Units and Property

6. **FloorPlans**
   - **Attributes**: FloorPlanID, PropertyID, Name, Bedrooms, Bathrooms, SquareFeet
   - **Relationships**: Linked to Units, SpaceOptions, PricingTiers, Specials/Discounts

7. **SpaceOptions**
   - **Attributes**: OptionID, FloorPlanID, Description, Size, Availability

8. **Leases**
   - **Attributes**: LeaseID, UnitID, CustomerID, LeaseTermID, StartDate, EndDate, LeaseStatus
   - **Relationships**: Linked to LeaseCharges, Renewals, LeaseTerms, ScheduledCharges, LeaseTermWindows

9. **Renewals**
   - **Attributes**: RenewalID, LeaseID, NewLeaseTermID, RenewalOfferDate, RenewalStatus

10. **LeaseCharges**
    - **Attributes**: ChargeID, LeaseID, ChargeCodeID, Amount, DueDate, Status

11. **Charge Codes**
    - **Attributes**: CodeID, Code, Description, Type, Category

12. **Scheduled Charges**
    - **Attributes**: ScheduleID, LeaseID, ChargeCodeID, Frequency, StartDate, EndDate

13. **Budgets**
    - **Attributes**: BudgetID, PropertyID, Year, BudgetAmount, ActualAmount

14. **Residents/Customers**
    - **Attributes**: CustomerID, Name, ContactInfoID, DemographicID, CreatedDate, Status

15. **Resident Demographics**
    - **Attributes**: DemographicID, Age, Income, Occupation, FamilySize

16. **Pricing Tiers**
    - **Attributes**: TierID, FloorPlanID, Season, Price, EffectiveDate, ExpirationDate

17. **Specials/Discounts**
    - **Attributes**: SpecialID, FloorPlanID, Description, DiscountType, Amount, StartDate, EndDate

18. **Unit Types**
    - **Attributes**: UnitTypeID, Description, Bedrooms, Bathrooms, Category

19. **Lease Terms**
    - **Attributes**: LeaseTermID, Duration, Description, EffectiveDate

20. **Lease Term Windows**
    - **Attributes**: WindowID, LeaseTermID, StartDate, EndDate

21. **Property Office and Pool Hours**
    - **Attributes**: HoursID, PropertyID, Type, Day, OpenTime, CloseTime

22. **Property Addresses**
    - **Attributes**: AddressID, PropertyID, Street, City, State, ZipCode, Country

23. **Property Contact Information**
    - **Attributes**: ContactInfoID, PropertyID, Phone, Email, Website, ContactPerson

24. **Property Geographic (GIS) Data**
    - **Attributes**: GISID, PropertyID, Latitude, Longitude, GeoJSON

25. **Property Profiles**
    - **Attributes**: ProfileID, PropertyID, ImageURL, BrandingDetails, URL

26. **Reports**
    - **Attributes**: ReportID, PropertyID, ReportType, CreatedDate, DataBlob
    - **Types**: Leasing Performance, Occupancy, Financials, Delinquency

27. **Leasing Performance**
    - **Attributes**: PerformanceID, PropertyID, Week, LeasingRate, TotalUnitsLeased

28. **Weekly Statistics**
    - **Attributes**: StatID, PropertyID, WeekNumber, MoveIns, MoveOuts, Renewals

29. **Occupancy Projections**
    - **Attributes**: ProjectionID, PropertyID, Month, ProjectedOccupancyRate

30. **Monthly Collections**
    - **Attributes**: CollectionID, PropertyID, Month, TotalCollected, OutstandingAmount

31. **Delinquency**
    - **Attributes**: DelinquencyID, PropertyID, LeaseID, AmountDue, DueDate

32. **Lead Data**
    - **Attributes**: LeadID, PropertyID, Source, ContactDate, FollowUpStatus

33. **Market Data**
    - **Attributes**: MarketDataID, PropertyID, CompetitorName, RateComparison, VacancyRate

34. **Model Beds**
    - **Attributes**: ModelBedID, PropertyID, Count, Description

35. **Lease History** (Current/Prior/Historical Lease)
    - **Attributes**: LeaseHistoryID, LeaseID, Status, CreatedDate, ModifiedDate

36. **Pre-Lease Data**
    - **Attributes**: PreLeaseID, PropertyID, CustomerID, ExpectedMoveInDate, LeaseTerm

37. **Performance Metrics**
    - **Attributes**: MetricID, PropertyID, Date, OccupancyRate, LeasingVelocity

38. **Images**
    - **Attributes**: ImageID, PropertyID, URL, Description, Tags, UploadDate

39. **Documents**
    - **Attributes**: DocumentID, PropertyID, Type, Description, FileURL, UploadedDate

**Relationships Overview:**
- **Portfolios** contain multiple **Properties** through **PortfolioAssignments**.
- **Properties** can contain multiple **Buildings**, **Units**, **FloorPlans**, etc.
- **Units** are linked to **Leases**, **Residents**, and **SpaceOptions**.
- **Leases** are linked to **LeaseCharges**, **ScheduledCharges**, and **LeaseTerms**.
- **Reports**, **Images**, and **Documents** are linked to **Properties** and provide detailed information and resources.

This structure can be implemented using a relational database, where foreign keys establish relationships between entities to ensure data integrity and consistency. The detailed attributes are placeholders and can be expanded depending on specific needs (e.g., adding audit fields like CreatedBy, UpdatedBy, etc.).