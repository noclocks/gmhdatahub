## Properties

### getProperties

- **Request URL**: https://gmhcommunities.entrata.com/api/v1/properties
- **Request HTTP Method**: `POST`
- **Status Codes**: `200`

#### Request

Headers:

```plaintext
Content-Type: APPLICATION/JSON; CHARSET=UTF-8
Authorization: Basic REDACTED
```

Requeset Body (Payload):

```json
{
	"auth": {
		"type" : "basic"
	},
	"requestId" : 15,
	"method": {
		"name": "getProperties",
		"version":"r1",
		"params": {
			"propertyIds" : "739076,739085",
			"propertyLookupCode" : "",
			"showAllStatus" : "1"
		}
	}
}
```

#### Response

```json
{
    "response": {
        "requestId": "15",
        "code": 200,
        "result": {
            "PhysicalProperty": {
                "Property": [
                    {
                        "PropertyID": 739076,
                        "MarketingName": "1008 S. 4th",
                        "Type": "Student",
                        "webSite": "https:\/\/www.theacademycampustown.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "1008 S. 4th",
                            "City": "Champaign",
                            "State": "IL",
                            "PostalCode": "61820",
                            "Country": "US",
                            "Email": "info@theacademycampustown.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "1008 S. 4th",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                },
                                {
                                    "AddressType": "Mailing",
                                    "Address": "708 South 6th Street",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "22",
                                    "Name": "Per Bedroom"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 10891,
                                    "Name": "2019\/2020",
                                    "TermMonths": 13,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 739085,
                        "MarketingName": "1047 Commonwealth Avenue",
                        "Type": "Student",
                        "webSite": "https:\/\/1047commonwealthavenue.prospectportal.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "1047 Commonwealth Avenue",
                            "City": "Boston",
                            "State": "MA",
                            "PostalCode": "02215",
                            "Country": "US",
                            "Email": "info@1047commonwealth.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "1047 Commonwealth Avenue",
                                    "City": "Boston",
                                    "StateCode": "MA",
                                    "PostalCode": "02215",
                                    "Country": "US"
                                }
                            ]
                        },
                        "Phone": {
                            "PhoneNumber": "6173510288",
                            "@attributes": {
                                "PhoneType": "primary"
                            },
                            "PhoneDescription": "Primary"
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11007,
                                    "Name": "Group",
                                    "TermMonths": 1,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10888,
                                    "Name": "6 months",
                                    "TermMonths": 6,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10887,
                                    "Name": "9 months",
                                    "TermMonths": 9,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 12060,
                                    "Name": "Academic Year",
                                    "TermMonths": 10,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9988,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "05\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 10886,
                                    "Name": "12 months",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11130,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10955,
                                    "Name": "2021 Lease Term",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10954,
                                    "Name": "2020 Lease Term",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10878,
                                    "Name": "2019\/2020",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11226,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11529,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9377,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11530,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9378,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11924,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9882,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11925,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9883,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 739079,
                        "ParentPropertyID": 739076,
                        "MarketingName": "307 E. Daniel",
                        "Type": "Student",
                        "webSite": "https:\/\/www.theacademycampustown.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "307 E. Daniel",
                            "City": "Champaign",
                            "State": "IL",
                            "PostalCode": "61820",
                            "Country": "US",
                            "Email": "info@theacademycampustown.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Mailing",
                                    "Address": "708 South 6th Street",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                },
                                {
                                    "AddressType": "Primary",
                                    "Address": "307 E. Daniel",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "By Appointment Only"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "33",
                                    "Name": "Standard"
                                },
                                {
                                    "Id": "22",
                                    "Name": "Per Bedroom"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11004,
                                    "Name": "2020\/2021 6 Month Lease",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                },
                                {
                                    "Id": 11008,
                                    "Name": "2020 Fall Semester",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 739080,
                        "ParentPropertyID": 739076,
                        "MarketingName": "501 S. 6th",
                        "Type": "Student",
                        "webSite": "https:\/\/www.theacademycampustown.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "501 S. 6th",
                            "City": "Champaign",
                            "State": "IL",
                            "PostalCode": "61820",
                            "Country": "US",
                            "Email": "Info@theacademycampustown.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Mailing",
                                    "Address": "708 South 6th Street",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                },
                                {
                                    "AddressType": "Primary",
                                    "Address": "501 S. 6th",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "By Appointment Only"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "22",
                                    "Name": "Per Bedroom"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11037,
                                    "Name": "2021 Fall Semester",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11009,
                                    "Name": "Spring Semester 2021",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11096,
                                    "Name": "2021\/2022 6 Month Lease",
                                    "TermMonths": 6,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11005,
                                    "Name": "2020\/2021 9 Month Lease",
                                    "TermMonths": 10,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 739084,
                        "ParentPropertyID": 739076,
                        "MarketingName": "908 S. 1st",
                        "Type": "Student",
                        "webSite": "https:\/\/www.theacademycampustown.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "908 S. 1st",
                            "City": "Champaign",
                            "State": "IL",
                            "PostalCode": "61820",
                            "Country": "US",
                            "Email": "Info@theacademycampustown.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Mailing",
                                    "Address": "708 South 6th Street",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                },
                                {
                                    "AddressType": "Primary",
                                    "Address": "908 S. 1st",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "By Appointment Only"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "22",
                                    "Name": "Per Bedroom"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11909,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9864,
                                                "WindowStartDate": "08\/15\/2025",
                                                "WindowEndDate": "07\/30\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11458,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9338,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/30\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 10894,
                                    "Name": "Immediate Move In 2020\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11121,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11910,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9868,
                                                "WindowStartDate": "08\/15\/2025",
                                                "WindowEndDate": "07\/30\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11020,
                                    "Name": "Immediate Move-In 2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11457,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9334,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/30\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11199,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 641240,
                        "MarketingName": "Academy 65",
                        "Type": "Student",
                        "YearBuilt": "2018",
                        "webSite": "https:\/\/www.academy65.com",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "1325 65th St",
                            "City": "Sacramento",
                            "State": "CA",
                            "PostalCode": "95819",
                            "Country": "US",
                            "Email": "info@academy65.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "1325 65th St",
                                    "City": "Sacramento",
                                    "StateCode": "CA",
                                    "PostalCode": "95819",
                                    "Country": "US"
                                }
                            ]
                        },
                        "Phone": {
                            "PhoneNumber": "9165271325",
                            "@attributes": {
                                "PhoneType": "primary"
                            },
                            "PhoneDescription": "Primary"
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "348",
                                    "Name": "Private Deluxe"
                                },
                                {
                                    "Id": "381",
                                    "Name": "2x2 Deluxe with Balcony"
                                },
                                {
                                    "Id": "15",
                                    "Name": "Lock-off"
                                },
                                {
                                    "Id": "16",
                                    "Name": "Standard Bedroom"
                                },
                                {
                                    "Id": "89",
                                    "Name": "Standard Bedroom No Window"
                                },
                                {
                                    "Id": "90",
                                    "Name": "Standard Bedroom w\/ Window"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "142",
                                    "Name": "Private Bathroom"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 10729,
                                    "Name": "2019\/2020",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                },
                                {
                                    "Id": 10901,
                                    "Name": "2020\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11132,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11027,
                                    "Name": "Immediate Move-In (2021\/2022)",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11573,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9423,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 12095,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 10023,
                                                "WindowStartDate": "08\/15\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11210,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 12096,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 10024,
                                                "WindowStartDate": "08\/15\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11574,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 676055,
                        "MarketingName": "Academy Lincoln",
                        "Type": "Student",
                        "webSite": "https:\/\/www.academylincoln.com\/academy-lincoln-lincoln-nebraska\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "1850 P Street",
                            "City": "Lincoln",
                            "State": "NE",
                            "PostalCode": "68508",
                            "Country": "US",
                            "Email": "info@academylincoln.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "1850 P Street",
                                    "City": "Lincoln",
                                    "StateCode": "NE",
                                    "PostalCode": "68508",
                                    "Country": "US"
                                }
                            ]
                        },
                        "Phone": {
                            "PhoneNumber": "4028589334",
                            "@attributes": {
                                "PhoneType": "primary"
                            },
                            "PhoneDescription": "Primary"
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "28",
                                    "Name": "Balcony"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11206,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10896,
                                    "Name": "2020\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11125,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11026,
                                    "Name": "Immediate Move In 2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10805,
                                    "Name": "2018\/2019",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10806,
                                    "Name": "2019\/2020",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11527,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9375,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11528,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9376,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11920,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9878,
                                                "WindowStartDate": "08\/16\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11921,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9879,
                                                "WindowStartDate": "08\/16\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                }
                            ]
                        },
                        "CustomKeysData": {
                            "CustomKeyData": [
                                {
                                    "Key": "PHONE",
                                    "Value": "(531) 329-6474"
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 1115679,
                        "MarketingName": "ANOVA uCity Square",
                        "Type": "Apartment",
                        "webSite": "https:\/\/www.anovaucity.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "3700 Lancaster Avenue",
                            "City": "Philadelphia",
                            "State": "PA",
                            "PostalCode": "19104",
                            "Country": "US",
                            "Email": "info@anovaucity.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "3700 Lancaster Avenue",
                                    "City": "Philadelphia",
                                    "StateCode": "PA",
                                    "PostalCode": "19104",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "1",
                                    "Name": "Conventional"
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 1161867,
                        "MarketingName": "Courts at Spring Mill Station",
                        "Type": "Apartment",
                        "YearBuilt": "2014",
                        "LongDescription": "Welcome home to The Courts at Spring Mill Station. Our brand new contemporary and comfortable apartments in Conshohocken are intuitively designed for style, functionality, convenience and historic charm, with inviting open-concept floor plans, abundant amenities, scenic views and garage parking. Close to transportation, shopping, restaurants and outdoor activities, our controlled access community is ideal for families, couples and individuals who appreciate a maintenance-free lifestyle in an upscale community with all the comforts of home and more.\r\nLocated on Hector Street in Conshohocken, The Courts at Spring Mill Station are just steps from the train to Center City in Philadelphia and other local attractions. We are also adjacent to the scenic Schuylkill Trail with views of the Schuylkill River. Walk to area bars, restaurants and shopping. Perfect your swing at the area&#039;s championship Golf Courses. Or stay right at home and take advantage of our clubroom, fitness center and resort-style swimming pool. \r\nFind the space that suites your lifestyle at The Courts at Spring Mill Station. We offer one and two bedroom apartments and lofts and apartments with dens, all designed to maximize space, storage and comfort. Experience contemporary living with open floor plans, granite countertops, stainless steel appliances and stacked washer\/dryer. Energy efficient oversized windows allow natural light to flow while you take in scenic views of the Schuylkill River, landscaped courtyards or resort-like pool. As part of our goal to become LEED certified, The Courts at Spring Mill Station also feature energy and water efficient appliances and smoke-free buildings.\r\nGet the most out of living at the Courts at Spring Mill Station. We offer activities for every age and interest. Relax in our large clubroom with games, high definition TVs and a fireplace. Pick up the pace in our fitness center, indoor basketball and volleyball courts or yoga and Pilates studio. Barbecue by the pool or gather in our dining area or Wi-Fi caf&eacute;. And remember, when you do venture out, the SEPTA Spring Mill Station with trains to Center City is just steps away.",
                        "webSite": "https:\/\/www.courtsatspringmill.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "1101 E. Hector Street, #104",
                            "City": "Conshohocken",
                            "State": "PA",
                            "PostalCode": "19428",
                            "Country": "US",
                            "Email": "info@courtsatspringmill.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "1101 E. Hector Street, #104",
                                    "City": "Conshohocken",
                                    "StateCode": "PA",
                                    "PostalCode": "19428",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "8:30 AM",
                                        "CloseTime": "5:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "8:30 AM",
                                        "CloseTime": "5:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "8:30 AM",
                                        "CloseTime": "5:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "8:30 AM",
                                        "CloseTime": "5:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "8:30 AM",
                                        "CloseTime": "5:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            },
                            "PoolHours": {
                                "PoolHour": [
                                    {
                                        "Day": "Monday",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "7:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "1",
                                    "Name": "Conventional"
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 518044,
                        "MarketingName": "Shortbread Lofts",
                        "Type": "Student",
                        "YearBuilt": "2014",
                        "webSite": "http:\/\/shortbreadlofts.com",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "333 W Rosemary Street",
                            "City": "Chapel Hill",
                            "State": "NC",
                            "PostalCode": "27516",
                            "Country": "US",
                            "Email": "info@shortbreadlofts.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "333 W Rosemary Street",
                                    "City": "Chapel Hill",
                                    "StateCode": "NC",
                                    "PostalCode": "27516",
                                    "Country": "US"
                                }
                            ]
                        },
                        "Phone": {
                            "PhoneNumber": "9199047640",
                            "@attributes": {
                                "PhoneType": "primary"
                            },
                            "PhoneDescription": "Primary"
                        },
                        "PostMonths": {
                            "ArPostMonth": "12\/2024",
                            "ApPostMonth": "12\/2024",
                            "GlPostMonth": "12\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "235",
                                    "Name": "Private Deluxe No Window"
                                },
                                {
                                    "Id": "37",
                                    "Name": "Private Top Floor Window"
                                },
                                {
                                    "Id": "236",
                                    "Name": "Private Deluxe Window"
                                },
                                {
                                    "Id": "38",
                                    "Name": "Shared Top Floor Window"
                                },
                                {
                                    "Id": "39",
                                    "Name": "Shared Window"
                                },
                                {
                                    "Id": "41",
                                    "Name": "Shared No Window"
                                },
                                {
                                    "Id": "42",
                                    "Name": "Private No Window"
                                },
                                {
                                    "Id": "40",
                                    "Name": "Private Window"
                                },
                                {
                                    "Id": "43",
                                    "Name": "Private Top Floor No Window"
                                },
                                {
                                    "Id": "44",
                                    "Name": "Shared Top Floor No Window"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "237",
                                    "Name": "Private Deluxe Top Floor No Window"
                                },
                                {
                                    "Id": "238",
                                    "Name": "Private Deluxe Top Floor Window"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 10895,
                                    "Name": "2020\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11123,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10342,
                                    "Name": "2018-2019",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11196,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11525,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9373,
                                                "WindowStartDate": "08\/08\/2024",
                                                "WindowEndDate": "07\/24\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11022,
                                    "Name": "2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10347,
                                    "Name": "Renewal Transfers",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10346,
                                    "Name": "Renewals 2017-2018",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10735,
                                    "Name": "2019\/2020",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11526,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9374,
                                                "WindowStartDate": "08\/08\/2024",
                                                "WindowEndDate": "07\/24\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11913,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9872,
                                                "WindowStartDate": "08\/08\/2025",
                                                "WindowEndDate": "07\/24\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11914,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9873,
                                                "WindowStartDate": "08\/08\/2025",
                                                "WindowEndDate": "07\/24\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 10343,
                                    "Name": "New Leases 2017-2018",
                                    "TermMonths": 24,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 952515,
                        "MarketingName": "SOVA",
                        "Type": "Student",
                        "YearBuilt": "2018",
                        "webSite": "https:\/\/sovaksu.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "3021 Hidden Forest  Ct",
                            "City": "Marietta",
                            "State": "GA",
                            "PostalCode": "30066",
                            "Country": "US",
                            "Email": "info@sovaksu.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "3021 Hidden Forest  Ct",
                                    "City": "Marietta",
                                    "StateCode": "GA",
                                    "PostalCode": "30066",
                                    "Country": "US"
                                }
                            ]
                        },
                        "Phone": {
                            "PhoneNumber": "8889102775",
                            "@attributes": {
                                "PhoneType": "primary"
                            },
                            "PhoneDescription": "Primary"
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "16",
                                    "Name": "Standard Bedroom"
                                },
                                {
                                    "Id": "130",
                                    "Name": "Deluxe Bedroom-Top Floor"
                                },
                                {
                                    "Id": "131",
                                    "Name": "Standard Bedroom-Top Floor"
                                },
                                {
                                    "Id": "133",
                                    "Name": "Standard-Pool View-Top Floor"
                                },
                                {
                                    "Id": "127",
                                    "Name": "Deluxe Bedroom"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11126,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10933,
                                    "Name": "Immediate Move-In 08\/15\/20-07\/31\/21",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11204,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11031,
                                    "Name": "Immediate Move-In 2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11532,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9381,
                                                "WindowStartDate": "08\/10\/2024",
                                                "WindowEndDate": "07\/28\/2025"
                                            },
                                            {
                                                "Id": 9382,
                                                "WindowStartDate": "08\/10\/2024",
                                                "WindowEndDate": "07\/28\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11917,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9876,
                                                "WindowStartDate": "08\/10\/2025",
                                                "WindowEndDate": "07\/26\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11919,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9877,
                                                "WindowStartDate": "08\/10\/2025",
                                                "WindowEndDate": "07\/26\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 10932,
                                    "Name": "2019\/2020",
                                    "TermMonths": 13,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 577897,
                        "MarketingName": "Station Nine",
                        "Type": "Apartment",
                        "YearBuilt": "2004",
                        "ShortDescription": "Modern, Loft-Style Luxury Apartments in Durham, NC",
                        "LongDescription": "Welcome to Station Nine, a premier apartment community, located in the heart of Ninth Street District, offering a true urban-living feel through our luxury apartments in Durham, NC! Our 1- and 2-bedroom apartments feature both open-concept and loft floor plans, giving you even bigger opportunities to make the place your own. Spacious interiors and modern design touches blend together with world-class amenities, including warm hardwood flooring, vaulted ceilings, large windows for lots of natural lighting, premium materials and finishes like granite counter tops, stainless steel appliances, an in-home washer and dryer, and private patio or balcony spaces so you can enjoy your natural surroundings at your leisure. As a resident, you&#039;ll also be able to relax by the resort-style pool, stay healthy in the fitness center, relax in the courtyards with friends, or grab a boost of energy from the coffee bar. Regardless of your lifestyle or living situation, Station Nine offers pet-friendly apartments with enough space and personality to keep you satisfied. We feel confident that you&#039;ll find your new home at Station Nine!",
                        "webSite": "https:\/\/www.stationnine.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "2211 Hillsborough Rd",
                            "City": "Durham",
                            "State": "NC",
                            "PostalCode": "27705",
                            "Country": "US",
                            "Email": "leasing@stationnine.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "2211 Hillsborough Rd",
                                    "City": "Durham",
                                    "StateCode": "NC",
                                    "PostalCode": "27705",
                                    "Country": "US"
                                }
                            ]
                        },
                        "Phone": {
                            "PhoneNumber": "9192863800",
                            "@attributes": {
                                "PhoneType": "primary"
                            },
                            "PhoneDescription": "Primary"
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            },
                            "PoolHours": {
                                "PoolHour": [
                                    {
                                        "Day": "Monday",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "9:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "9:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "9:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "9:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "9:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "9:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "OpenTime": "12:00 AM",
                                        "CloseTime": "9:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "1",
                                    "Name": "Conventional"
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 518041,
                        "MarketingName": "The Academy at Frisco",
                        "Type": "Student",
                        "YearBuilt": "2012",
                        "webSite": "http:\/\/www.theacademyatfrisco.com",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "413 N West Ave",
                            "City": "Fayetteville",
                            "State": "AR",
                            "PostalCode": "72701",
                            "Country": "US",
                            "Email": "info@theacademyatfrisco.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "413 N West Ave",
                                    "City": "Fayetteville",
                                    "StateCode": "AR",
                                    "PostalCode": "72701",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "23",
                                    "Name": "By Unit (Two Bedroom)"
                                },
                                {
                                    "Id": "16",
                                    "Name": "Standard Bedroom"
                                },
                                {
                                    "Id": "58",
                                    "Name": "Street Access"
                                },
                                {
                                    "Id": "57",
                                    "Name": "Deluxe Balcony"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "60",
                                    "Name": "Pool View"
                                },
                                {
                                    "Id": "138",
                                    "Name": "Balcony"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 10658,
                                    "Name": "Short Term Renewal",
                                    "TermMonths": 6,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11025,
                                    "Name": "Immediate Move In 2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10114,
                                    "Name": "2018-2019",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10719,
                                    "Name": "2019\/2020",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11202,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11124,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11531,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9379,
                                                "WindowStartDate": "08\/15\/2024",
                                                "WindowEndDate": "07\/23\/2025"
                                            },
                                            {
                                                "Id": 9380,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/23\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11916,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9874,
                                                "WindowStartDate": "08\/11\/2025",
                                                "WindowEndDate": "07\/23\/2026"
                                            },
                                            {
                                                "Id": 9875,
                                                "WindowStartDate": "08\/12\/2025",
                                                "WindowEndDate": "07\/23\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 12162,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 10090,
                                                "WindowStartDate": "08\/11\/2025",
                                                "WindowEndDate": "07\/23\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 10890,
                                    "Name": "2020\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10078,
                                    "Name": "2017\/2018",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                },
                                {
                                    "Id": 10079,
                                    "Name": "Current\/Migration Only",
                                    "TermMonths": 31,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 518042,
                        "MarketingName": "The Academy on Charles",
                        "Type": "Student",
                        "YearBuilt": "2012",
                        "webSite": "http:\/\/www.theacademyoncharles.com",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "3700 N Charles St",
                            "City": "Baltimore",
                            "State": "MD",
                            "PostalCode": "21218",
                            "Country": "US",
                            "Email": "info@theacademyoncharles.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "3700 N Charles St",
                                    "City": "Baltimore",
                                    "StateCode": "MD",
                                    "PostalCode": "21218",
                                    "Country": "US"
                                }
                            ]
                        },
                        "Phone": {
                            "PhoneNumber": "4435632231",
                            "@attributes": {
                                "PhoneType": "primary"
                            },
                            "PhoneDescription": "Primary"
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "16",
                                    "Name": "Standard Bedroom"
                                },
                                {
                                    "Id": "62",
                                    "Name": "Deluxe w\/ Private Bathroom"
                                },
                                {
                                    "Id": "63",
                                    "Name": "Super Deluxe w\/ Private Bathroom"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "151",
                                    "Name": "Standard Bedroom w\/ Floor Premium"
                                },
                                {
                                    "Id": "153",
                                    "Name": "Super Deluxe w\/ Floor Premium"
                                },
                                {
                                    "Id": "152",
                                    "Name": "Deluxe w\/ Floor Premium"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 10080,
                                    "Name": "2017\/2018",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11201,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10899,
                                    "Name": "2020\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11127,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10662,
                                    "Name": "June 2018 to May 2019",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                },
                                {
                                    "Id": 11032,
                                    "Name": "2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10773,
                                    "Name": "2019\/2020",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10081,
                                    "Name": "Current\/Migration Only",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10323,
                                    "Name": "2018\/2019",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11567,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9417,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/26\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11568,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9418,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/26\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11922,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9880,
                                                "WindowStartDate": "08\/16\/2025",
                                                "WindowEndDate": "07\/26\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11923,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9881,
                                                "WindowStartDate": "08\/16\/2025",
                                                "WindowEndDate": "07\/26\/2026"
                                            }
                                        ]
                                    }
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 1197886,
                        "MarketingName": "The Caswell at Runnymeade",
                        "Type": "Apartment",
                        "YearBuilt": "2023",
                        "webSite": "https:\/\/www.thecaswellresidences.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "1000 Bluebird View",
                            "City": "Newtown Square",
                            "State": "PA",
                            "PostalCode": "19073",
                            "Country": "US",
                            "Email": "info@thecaswellresidences.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "1000 Bluebird View",
                                    "City": "Newtown Square",
                                    "StateCode": "PA",
                                    "PostalCode": "19073",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "5:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "5:30 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "5:30 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "5:30 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "5:30 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "5:30 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "1",
                                    "Name": "Conventional"
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 833617,
                        "MarketingName": "The Dean Campustown",
                        "Type": "Student",
                        "YearBuilt": "2020",
                        "ShortDescription": "The Dean offers unique student apartments near UIUC you can&#039;t find anywhere else. Here, convenience and luxury culminate in the best housing amenities, including upscale apartment features. Contact us today to find out details on our 16th a",
                        "LongDescription": "Take your college living experience to the next level with The Dean in Champaign. Conveniently located in the center of Campustown and adjacent to the heart of campus and Greek life, our student apartments near the University of Illinois at Urbana-Champaign are comprised of everything students need and want &mdash; including spacious apartments with modern finishes and a range of luxury amenities.",
                        "webSite": "https:\/\/www.thedean.com\/campustown\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "708 South 6th Street",
                            "City": "Champaign",
                            "State": "IL",
                            "PostalCode": "61820",
                            "Country": "US",
                            "Email": "info@thedeancampustown.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "708 South 6th Street",
                                    "City": "Champaign",
                                    "StateCode": "IL",
                                    "PostalCode": "61820",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "134",
                                    "Name": "Extra Large 2x2"
                                },
                                {
                                    "Id": "135",
                                    "Name": "Skyline"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "157",
                                    "Name": "Extra Large 2x2 - Skyline"
                                },
                                {
                                    "Id": "158",
                                    "Name": "Extra Large 2x2 - Aerial"
                                },
                                {
                                    "Id": "159",
                                    "Name": "Extra Large 2x2 - Penthouse"
                                },
                                {
                                    "Id": "33",
                                    "Name": "Standard"
                                },
                                {
                                    "Id": "28",
                                    "Name": "Balcony"
                                },
                                {
                                    "Id": "26",
                                    "Name": "Aerial"
                                },
                                {
                                    "Id": "27",
                                    "Name": "Penthouse"
                                },
                                {
                                    "Id": "163",
                                    "Name": "Extra Large 1x1 - Skyline"
                                },
                                {
                                    "Id": "164",
                                    "Name": "Extra Large 1x1 - Aerial"
                                },
                                {
                                    "Id": "165",
                                    "Name": "Extra Large 1x1 - Penthouse"
                                },
                                {
                                    "Id": "167",
                                    "Name": "Extra Large 0 x 1 Skyline"
                                },
                                {
                                    "Id": "168",
                                    "Name": "Extra Large 0 x 1 Aerial"
                                },
                                {
                                    "Id": "169",
                                    "Name": "Extra Large 0 x 1 Penthouse"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11036,
                                    "Name": "2021 Fall Semester",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11011,
                                    "Name": "2020 Fall Semester",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11014,
                                    "Name": "2021 Spring Semester",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11095,
                                    "Name": "2021\/2022 6 Month Lease",
                                    "TermMonths": 6,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11120,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11010,
                                    "Name": "2020\/2021 Full Term",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11094,
                                    "Name": "2021\/2022 Academic",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11456,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9329,
                                                "WindowStartDate": "08\/19\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            },
                                            {
                                                "Id": 9330,
                                                "WindowStartDate": "08\/20\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 10884,
                                    "Name": "Immediate Move In - 07\/31\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11019,
                                    "Name": "Immediate Move In 2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11911,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9869,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11912,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9871,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11200,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 1197887,
                        "MarketingName": "The Dean Reno",
                        "Type": "Student",
                        "YearBuilt": "2023",
                        "webSite": "http:\/\/thedeanreno.prospectportal.com",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "1475 N. Virginia Street",
                            "City": "Reno",
                            "State": "NV",
                            "PostalCode": "89503",
                            "Country": "US",
                            "Email": "info@thedeanreno.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "1475 N. Virginia Street",
                                    "City": "Reno",
                                    "StateCode": "NV",
                                    "PostalCode": "89503",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "480",
                                    "Name": "Penthouse Deluxe"
                                },
                                {
                                    "Id": "273",
                                    "Name": "Twin, Amenity Deck"
                                },
                                {
                                    "Id": "274",
                                    "Name": "Twin"
                                },
                                {
                                    "Id": "275",
                                    "Name": "Twin, Aerial Level"
                                },
                                {
                                    "Id": "60",
                                    "Name": "Pool View"
                                },
                                {
                                    "Id": "210",
                                    "Name": "Aerial Level w\/ Pool View & Adjoining Bath"
                                },
                                {
                                    "Id": "214",
                                    "Name": "Amenity Deck"
                                },
                                {
                                    "Id": "216",
                                    "Name": "Amenity Deck w\/Patio"
                                },
                                {
                                    "Id": "218",
                                    "Name": "Amenity Deck w\/Patio & Pool View"
                                },
                                {
                                    "Id": "220",
                                    "Name": "Amenity Deck w\/Pool View"
                                },
                                {
                                    "Id": "224",
                                    "Name": "Pool View & Adjoining Bath"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "207",
                                    "Name": "Aerial Level"
                                },
                                {
                                    "Id": "208",
                                    "Name": "Penthouse Level"
                                },
                                {
                                    "Id": "209",
                                    "Name": "Adjoining Bath"
                                },
                                {
                                    "Id": "212",
                                    "Name": "Aerial Level w\/Adjoining Bath"
                                },
                                {
                                    "Id": "213",
                                    "Name": "Aerial Level w\/Pool View"
                                },
                                {
                                    "Id": "215",
                                    "Name": "Amenity Deck w\/Adjoining Bath"
                                },
                                {
                                    "Id": "217",
                                    "Name": "Amenity Deck w\/Patio & Adjoining Bath"
                                },
                                {
                                    "Id": "219",
                                    "Name": "Amenity Deck w\/Patio, Pool View & Adjoining Bath"
                                },
                                {
                                    "Id": "221",
                                    "Name": "Penthouse Level w\/Adjoining Bath"
                                },
                                {
                                    "Id": "222",
                                    "Name": "Penthouse Level w\/Pool View"
                                },
                                {
                                    "Id": "223",
                                    "Name": "Penthouse Level w\/Pool View & Adjoining Bath"
                                },
                                {
                                    "Id": "187",
                                    "Name": "Shared, Aerial Level"
                                },
                                {
                                    "Id": "225",
                                    "Name": "Shared, Aerial Level w\/Pool View"
                                },
                                {
                                    "Id": "226",
                                    "Name": "Shared, Amenity Deck"
                                },
                                {
                                    "Id": "227",
                                    "Name": "Shared, Amenity Deck w\/Pool View"
                                },
                                {
                                    "Id": "228",
                                    "Name": "Shared, Penthouse Level"
                                },
                                {
                                    "Id": "229",
                                    "Name": "Shared, Penthouse Level w\/Pool View"
                                },
                                {
                                    "Id": "230",
                                    "Name": "Shared, Pool View"
                                },
                                {
                                    "Id": "276",
                                    "Name": "Twin, Penthouse Level"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11958,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9919,
                                                "WindowStartDate": "08\/16\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11575,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9425,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            },
                                            {
                                                "Id": 9661,
                                                "WindowStartDate": "08\/18\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11576,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9426,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11959,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9921,
                                                "WindowStartDate": "08\/16\/2025",
                                                "WindowEndDate": "07\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11212,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 518046,
                        "MarketingName": "The Rise at Northgate",
                        "Type": "Student",
                        "YearBuilt": "2013",
                        "webSite": "http:\/\/www.riseatnorthgate.com",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "717 University Dr",
                            "City": "College Station",
                            "State": "TX",
                            "PostalCode": "77840",
                            "Country": "US",
                            "Email": "info@riseatnorthgate.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "717 University Dr",
                                    "City": "College Station",
                                    "StateCode": "TX",
                                    "PostalCode": "77840",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "7:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "9:00 AM",
                                        "CloseTime": "7:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "142",
                                    "Name": "Private Bathroom"
                                },
                                {
                                    "Id": "16",
                                    "Name": "Standard Bedroom"
                                },
                                {
                                    "Id": "56",
                                    "Name": "Balcony Standard Bedroom"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "47",
                                    "Name": "Balcony Private Bathroom"
                                },
                                {
                                    "Id": "52",
                                    "Name": "Aerial Standard Bedroom"
                                },
                                {
                                    "Id": "45",
                                    "Name": "Aerial Private Bathroom"
                                },
                                {
                                    "Id": "54",
                                    "Name": "Aerial Balcony Standard Bedroom"
                                },
                                {
                                    "Id": "50",
                                    "Name": "Aerial Balcony Private Bathroom"
                                },
                                {
                                    "Id": "53",
                                    "Name": "Penthouse Standard Bedroom"
                                },
                                {
                                    "Id": "46",
                                    "Name": "Penthouse Private Bathroom"
                                },
                                {
                                    "Id": "55",
                                    "Name": "Penthouse Balcony Standard Bedroom"
                                },
                                {
                                    "Id": "51",
                                    "Name": "Penthouse Balcony Private Bathroom"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 10361,
                                    "Name": "2016\/2017 Renewal",
                                    "TermMonths": 1,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10365,
                                    "Name": "Fall Semester 2017 2018",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10338,
                                    "Name": "Fall New Lease 2018",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                },
                                {
                                    "Id": 10364,
                                    "Name": "Fall 2018 Renewal",
                                    "TermMonths": 6,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10336,
                                    "Name": "2017\/2018",
                                    "TermMonths": 7,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                },
                                {
                                    "Id": 10366,
                                    "Name": "Spring Semester 2018",
                                    "TermMonths": 7,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10359,
                                    "Name": "2016\/2017 New Lease Pent & Aerial",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                },
                                {
                                    "Id": 11566,
                                    "Name": "Transfer 2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9416,
                                                "WindowStartDate": "08\/05\/2024",
                                                "WindowEndDate": "07\/20\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 12093,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 10021,
                                                "WindowStartDate": "08\/04\/2025",
                                                "WindowEndDate": "07\/19\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 12094,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 10022,
                                                "WindowStartDate": "08\/04\/2025",
                                                "WindowEndDate": "07\/19\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11203,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11565,
                                    "Name": "2024\/2025",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9415,
                                                "WindowStartDate": "08\/05\/2024",
                                                "WindowEndDate": "07\/20\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 10897,
                                    "Name": "2020\/2021",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11122,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10337,
                                    "Name": "2018\/2019",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10363,
                                    "Name": "2017\/2018 Renewals",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10772,
                                    "Name": "2019\/2020",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10362,
                                    "Name": "2016\/2017 Renewal Transfer",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11033,
                                    "Name": "2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 10360,
                                    "Name": "2016\/2017 New Lease Sky & Terrace",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": false
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 1143679,
                        "MarketingName": "Torre",
                        "Type": "Student",
                        "YearBuilt": "1969",
                        "webSite": "http:\/\/www.torreatx.com\/",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "2020 Nueces St",
                            "City": "Austin",
                            "State": "TX",
                            "PostalCode": "78705",
                            "Country": "US",
                            "Email": "info@torreatx.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "2020 Nueces St",
                                    "City": "Austin",
                                    "StateCode": "TX",
                                    "PostalCode": "78705",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "4:00 PM"
                                    },
                                    {
                                        "Day": "Sunday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "12:00 PM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "447",
                                    "Name": "Super Single, Penthouse Level w\/Terrace"
                                },
                                {
                                    "Id": "182",
                                    "Name": "Private, Skyline Level w\/Terrace"
                                },
                                {
                                    "Id": "170",
                                    "Name": "Private, Aerial Level"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "172",
                                    "Name": "Private, Aerial Level w\/Downtown View"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "176",
                                    "Name": "Private, Penthouse Level"
                                },
                                {
                                    "Id": "195",
                                    "Name": "Shared, Terrace"
                                },
                                {
                                    "Id": "197",
                                    "Name": "Shared, Terrace & Downtown View"
                                },
                                {
                                    "Id": "178",
                                    "Name": "Private, Penthouse Level w\/Downtown View"
                                },
                                {
                                    "Id": "187",
                                    "Name": "Shared, Aerial Level"
                                },
                                {
                                    "Id": "174",
                                    "Name": "Private, Downtown View"
                                },
                                {
                                    "Id": "184",
                                    "Name": "Private, Terrace"
                                },
                                {
                                    "Id": "186",
                                    "Name": "Private, Terrace & Downtown View"
                                },
                                {
                                    "Id": "190",
                                    "Name": "Shared, Downtown View"
                                },
                                {
                                    "Id": "191",
                                    "Name": "Shared, Penthouse Level w\/Downtown View"
                                },
                                {
                                    "Id": "192",
                                    "Name": "Shared, Skyline Level"
                                },
                                {
                                    "Id": "181",
                                    "Name": "Private, Skyline Level w\/Downtown View"
                                },
                                {
                                    "Id": "179",
                                    "Name": "Private, Skyline Level"
                                },
                                {
                                    "Id": "198",
                                    "Name": "Shared, Skyline Level w\/Downtown View"
                                },
                                {
                                    "Id": "199",
                                    "Name": "Super Single"
                                },
                                {
                                    "Id": "201",
                                    "Name": "Super Single, Skyline Level"
                                },
                                {
                                    "Id": "203",
                                    "Name": "Super Single, Aerial Level"
                                },
                                {
                                    "Id": "205",
                                    "Name": "Private, Penthouse Level w\/Terrace"
                                },
                                {
                                    "Id": "204",
                                    "Name": "Shared, Skyline Level w\/Terrace"
                                },
                                {
                                    "Id": "206",
                                    "Name": "Shared, Aerial Level w\/Downtown View"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 11810,
                                    "Name": "Fall Short Term Leases",
                                    "TermMonths": 5,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9728,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "12\/30\/2024"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11809,
                                    "Name": "Academic Year Leases",
                                    "TermMonths": 10,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9727,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "05\/31\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11491,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9339,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/30\/2025"
                                            },
                                            {
                                                "Id": 9954,
                                                "WindowStartDate": "08\/17\/2025",
                                                "WindowEndDate": "07\/30\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11141,
                                    "Name": "2022\/2023",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11140,
                                    "Name": "2021\/2022",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                },
                                {
                                    "Id": 11492,
                                    "Name": "MOMI",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9340,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "07\/30\/2025"
                                            },
                                            {
                                                "Id": 9795,
                                                "WindowStartDate": "08\/16\/2024",
                                                "WindowEndDate": "05\/31\/2025"
                                            },
                                            {
                                                "Id": 9955,
                                                "WindowStartDate": "08\/17\/2025",
                                                "WindowEndDate": "07\/30\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11211,
                                    "Name": "2023\/2024",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true
                                }
                            ]
                        }
                    },
                    {
                        "PropertyID": 1311849,
                        "MarketingName": "Venue at North Campus",
                        "Type": "Student",
                        "YearBuilt": "2012",
                        "ShortDescription": "The Venue at North Campus is redefining student living with our variety of modern apartments for University of South Florida students! \r\nWe offer studio, 2, 4, and 5 bedroom apartments and a wide range of features and amenities.",
                        "LongDescription": "We admit it &mdash; we&#039;re extra. At Venue at North Campus, we take student living to a new level. Located in the heart of Tampa, Florida, VNC offers off-campus student housing that provides you with extra college experiences. From stylish studios to modern townhomes, we have extra apartment options available to choose from. All apartments come fully-furnished and are ready for you to make yourself home.",
                        "webSite": "http:\/\/venueatnorthcampusfl.prospectportal.com",
                        "Address": {
                            "@attributes": {
                                "AddressType": "property"
                            },
                            "Address": "13702 North 42nd St",
                            "City": "Tampa",
                            "State": "FL",
                            "PostalCode": "33613",
                            "Country": "US",
                            "Email": "info@venueatnorthcampus.com"
                        },
                        "Addresses": {
                            "Address": [
                                {
                                    "AddressType": "Primary",
                                    "Address": "13702 North 42nd St",
                                    "City": "Tampa",
                                    "StateCode": "FL",
                                    "PostalCode": "33613",
                                    "Country": "US"
                                }
                            ]
                        },
                        "PostMonths": {
                            "ArPostMonth": "11\/2024",
                            "ApPostMonth": "11\/2024",
                            "GlPostMonth": "11\/2024"
                        },
                        "PropertyHours": {
                            "OfficeHours": {
                                "OfficeHour": [
                                    {
                                        "Day": "Monday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Tuesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Friday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Wednesday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Thursday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "10:00 AM",
                                        "CloseTime": "6:00 PM"
                                    },
                                    {
                                        "Day": "Saturday",
                                        "AvailabilityType": "Open",
                                        "OpenTime": "11:00 AM",
                                        "CloseTime": "4:00 PM"
                                    }
                                ]
                            }
                        },
                        "IsDisabled": 0,
                        "IsFeaturedProperty": 0,
                        "SpaceOptions": {
                            "SpaceOption": [
                                {
                                    "Id": "33",
                                    "Name": "Standard"
                                },
                                {
                                    "Id": "2",
                                    "Name": "Private"
                                },
                                {
                                    "Id": "3",
                                    "Name": "Shared"
                                },
                                {
                                    "Id": "60",
                                    "Name": "Pool View"
                                },
                                {
                                    "Id": "315",
                                    "Name": "Backyard"
                                },
                                {
                                    "Id": "309",
                                    "Name": "Courtyard View"
                                }
                            ]
                        },
                        "LeaseTerms": {
                            "LeaseTerm": [
                                {
                                    "Id": 12129,
                                    "Name": "Spring Lease Term",
                                    "TermMonths": 7,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 10057,
                                                "WindowStartDate": "01\/03\/2025",
                                                "WindowEndDate": "07\/25\/2025"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11876,
                                    "Name": "Academic Year MO\/MI Lease Term",
                                    "TermMonths": 10,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9828,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "05\/31\/2025"
                                            },
                                            {
                                                "Id": 10124,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "05\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11709,
                                    "Name": "Academic Year",
                                    "TermMonths": 10,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9694,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "05\/31\/2025"
                                            },
                                            {
                                                "Id": 10123,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "05\/31\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11843,
                                    "Name": "MO\/MI Lease Term",
                                    "TermMonths": 12,
                                    "IsProspect": false,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9762,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/25\/2025"
                                            },
                                            {
                                                "Id": 9885,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "07\/25\/2026"
                                            }
                                        ]
                                    }
                                },
                                {
                                    "Id": 11643,
                                    "Name": "Annual",
                                    "TermMonths": 12,
                                    "IsProspect": true,
                                    "IsRenewal": true,
                                    "LeaseStartWindows": {
                                        "LeaseStartWindow": [
                                            {
                                                "Id": 9495,
                                                "WindowStartDate": "08\/17\/2024",
                                                "WindowEndDate": "07\/25\/2025"
                                            },
                                            {
                                                "Id": 9884,
                                                "WindowStartDate": "08\/18\/2025",
                                                "WindowEndDate": "07\/25\/2026"
                                            }
                                        ]
                                    }
                                }
                            ]
                        }
                    }
                ]
            }
        }
    }
}
```
