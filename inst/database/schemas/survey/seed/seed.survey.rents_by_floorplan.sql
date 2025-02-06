WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.rents_by_floorplan (
    property_id,
    leasing_week_id,
    property_name,
    floorplan_type,
    floorplan_id,
    square_feet,
    number_of_beds,
    number_of_baths,
    total_units_count,
    available,
    market_rent_per_bed,
    concessions_gift_card,
    concessions_one_time_rent,
    concessions_monthly_rent,
    expenses_furniture,
    expenses_tv,
    expenses_electricity_gas,
    expenses_water,
    expenses_cable_internet,
    expenses_trash_valet,
    expenses_parking,
    created_by,
    updated_by
)
SELECT
    property_id,
    leasing_week_id,
    property_name,
    floorplan_type,
    floorplan_id,
    square_feet,
    number_of_beds,
    number_of_baths,
    total_units_count,
    available,
    market_rent_per_bed,
    concessions_gift_card,
    concessions_one_time_rent,
    concessions_monthly_rent,
    expenses_furniture,
    expenses_tv,
    expenses_electricity_gas,
    expenses_water,
    expenses_cable_internet,
    expenses_trash_valet,
    expenses_parking,
    (SELECT user_id FROM user_id),
    (SELECT user_id FROM user_id)
FROM (
    VALUES
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S1', 312.5, 1, 1, 25, true, 2895.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S2', 316.0, 1, 1, 12, true, 2972.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S3', 335.5, 1, 1, 51, true, 3080.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S4', 323.5, 1, 1, 8, true, 3025.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S5', 343.5, 1, 1, 10, true, 3150.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S6', 362.5, 1, 1, 29, true, 3350.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S7', 444.0, 1, 1, 4, true, 3410.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S8', 344.0, 1, 1, 11, true, 3422.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S9', 399.5, 1, 1, 5, true, 3975.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S10', 528.0, 1, 1, 4, true, 4037.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        (739085, 6, '1047 Commonwealth Avenue', 'Studio', 'S11', 339.0, 1, 1, 24, false, 1196.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
) AS v(property_id, leasing_week_id, property_name, floorplan_type, floorplan_id, square_feet, number_of_beds, number_of_baths, total_units_count,
available, market_rent_per_bed, concessions_gift_card, concessions_one_time_rent, concessions_monthly_rent, expenses_furniture, expenses_tv, expenses_electricity_gas, expenses_water, expenses_cable_internet, expenses_trash_valet, expenses_parking);
