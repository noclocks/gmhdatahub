# GMH Project Status and Architecture Overview Audit

## Contents

[TOC]

> [!NOTE]
> *In order to properly address the complexities of this project, I will first outline the existing components and architectural design of the GMH project. This will help me to understand the current state of the project and identify any potential issues or areas for improvement.*

## Current Architecture Overview

The current GMH project consists of several components that are designed to work together to provide a leasing dashboard Shiny app.

The main components of the project are as follows:

- Leasing Dashboard Shiny App (`gmhLeasingDashboard`)
- Authentication System: (`noClocksAuthR` and `noClocksAuthPlumber`)
    - Plumber API (included in `noClocksAuthPlumber`)
    - Admin Interface Shiny App (included with `noClocksAuthR`)

- Databases:
    - Auth database (hosted on Supabase)
    - Leasing Data database (hosted on GCP)

### GMH Leasing Dashboard Shiny App (`gmhLeasingDashboard`)

- Packaged in an R Package: `gmhLeasingDashboard`
- Hosted on Google Cloud Run in a Docker Container
- Pulls and processes data from the Entrata API in real-time.

### Authentication System

- Plumber API: 
    - CRUD for Users, Apps, etc.
    - Provides auth layer for shiny apps via server-side sessions and HTTP cookies.
    - Hosted on Google Cloud Run in a Docker Container

- Admin Interface Shiny App:
    - Provides a user interface for managing users, roles, etc.
    - Can be navigated to and from via a dedicated shiny module for the admin button
    - Hosted on Google Cloud Run in a Docker Container

- R Functions for securing Shiny apps (i.e. `secure_ui` and `secure_server`, etc.)

- Both the Plumber API and the Admin Interface are packaged in R packages and run in Docker containers on Google Cloud Run.

### Databases

- Auth database hosted on Supabase (PostgreSQL).

- Leasing Data database hosted on GCP (PostgreSQL).


### Data Processing

- Real-time data pulling and processing from the Entrata API within the Shiny app.
- No dedicated ETL (Extract, Transform, Load) process or data warehousing.

## Identified Challenges

### Overly Complex Architecture

-   Multiple, separate R packages and Docker container services that increase technical debt and maintenance overhead
-   Lack of any CI/CD or infrastructure as code to help maintain and deploy cloud services
-   Separate databases for authentication and leasing data complicate data management

### Data Processing and Performance Issues

-   Real time data processing of Entrata API data withing the Leasing Dashboard Shiny App leads to slow startup, increased resource consumption, and unnecessary data processing.
-   Users may experience delays due to on-the-fly data retrieval and processing.
-   Lack of any concrete documentation for the data processing pipelines, data sources, and data models.
-   No dedicated ETL process or data warehousing for managing and processing data.

### Authentication Complexity

-   Custom authentication system adds deployment and integration burdens
-   Legacy codebase does not align with current best practices for authentication and security standards
-   Lack of any documentation for the authentication system and its components
-   No automated testing for the authentication system

### Data Persistence

-   Lack of any unified data storage strategy for historical data, logging and monitoring, and user driven input data
-   Potential data redundancy and synchronization issues

## Proposed Architecture Improvements

### Consolidate Databases

-   Unify the authentication and leasing data databases into a single database to simplify data management and reduce complexity.
-   Host this database on GCP to leverage existing infrastructure and services.
-   Benefits:
    -   Simplifies data access management and backup processes
    -   Reduces latency and potential synchronization issues between databases
    -   Streamlines data processing, querying, and administrative tasks

### Implement Dedicated ETL Process

-   Develop a dedicated ETL process to extract, transform, and load data from the Entrata API into the unified database.

-   **Scheduled Data Ingestion**:

    -   Implement scheduled data ingestion to pull data from the Entrata API at regular intervals.
    -   Store this data in a staging area for further processing.
    -   Store processed, summary data in unified database for dashboard consumption.

-   **Benefits**:

    -   Reduces data processing overhead within the Shiny app
    -   Offloads data processing resources to dedicated ETL processes
    -   Provides historical data analysis capabilities
    -   Enhances data quality and consistency

#### Simplify Authentication System

Options:

-   Adopt a Managed Authentication Service or GCP built in authentication:
    -   Replace custom authentication system with a standardized, secure authentication service such as Auth0 or Firebase Authentication.
-   Revamp existing authentication system into a dedicated system for GMH
       -   Refactor existing authentication code to align with current best practices and security standards.
       -   Combine separate R packages into a single package for easier maintenance and deployment
       -   Centralize authentication system’s documentation

#### Refactor and Enhance Existing Shiny Apps

-   **UI/UX**:

    -   Refactor the existing Shiny app to improve user experience and performance.

    -   Use modern packages (i.e. `bslib`) for UI theming and styling and conform to GMH’s branding guidelines.

    -   Optimize data visualization and interactivity for better user engagement.

-   **Modularize**:

    -   Use Shiny Modules to separate concerns withing the apps
    -   Create reusable components for UI and server logic

-   **Optimize Data Access**:

    -   Modify app to read from database or a separate API instead of real-time data processing.
    -   Implement caching mechanisms for frequently accessed data.

#### Streamline Architecture

-   **Consolidate and Document Cloud Services**:
    -   Host all components of the system (Shiny Apps, Plumber APIs, ETL Processes, Databases, Secrets, etc.) on GCP.
    -   Create a dedicated project for GMH on GCP to manage all services.
    -   Create a separate, dedicated billing account linked to the new GCP project for GMH to manage costs and resources.
    -   Document all services, configurations, and integrations for future reference and onboarding.Impl
-   **CI/CD and Infrastructure as Code**:
   -   Implement CI/CD pipelines for automated testing, building, and deployment of services.
   -   Use Terraform or `gcloud`/R scripts to define and manage infrastructure as code.
   -   Automate service provisioning and deployment via GitHub Actions or Google Cloud Build

## High Level System Architecture

-   **Data Layer**:
    -   **Unified PostgreSQL Database** on GCP storing both authentication data and leasing data.
-   **ETL Process**:
    -   Scheduled jobs fetching data from the **Entrata API**.
    -   Data transformation and loading into the PostgreSQL database.
-   **Authentication Service**:
    -   Managed authentication (e.g., Auth0) or a refactored custom solution.
    -   Handles user management, authentication flows, and session management.
-   **Application Layer**:
    -   **GMH Leasing Dashboard Shiny App** accessing pre-processed data from the database.
    -   Hosted on Google Cloud Run or GKE for scalability.
-   **User Interface**:
    -   Web-based dashboard with optimized performance due to pre-loaded data.

## Implementation Roadmap

**Phase 1: Planning**

-   Assess Requirements:
    -   Determine the necessity of custom authentication versus managed services.
    -   Define data models and schemas for the unified database.
    -   Plan the ETL process schedule and data retention policies.

**Phase 2: Database Consolidation**

-   Set Up Unified Database:
    -   Provision a PostgreSQL instance on GCP.
    -   Migrate data from Supabase to GCP.
    -   Update database connection strings in the application code.

**Phase 3: Implement ETL Process**:
    
- **Develop ETL Scripts**:
    -   Write R scripts or use a tool like Apache Airflow.
    -   Schedule data pulls from the Entrata API.
    -   Include error handling and logging.

-   **Data Validation**:
    -   Implement checks to ensure data integrity.
    -   Handle discrepancies and notify stakeholders.

**Phase 4: Authentication System Overhaul**

-   **Managed Service Integration**:
    -   If adopting a managed service, integrate it with the Shiny app.
    -   Update the authentication flow in the app code.

-   **Refactor Custom Auth (if applicable)**:
    -   Simplify and modernize authentication code.
    -   Combine auth packages and remove redundant code.

**Phase 5: Shiny App Refactoring**

-   **Code Modularization**:
    -   Break down the app into modules for better maintainability.
    -   Refactor UI and server logic to separate concerns.

-   **Performance Optimization**:
    -   Remove real-time API calls.
    -   Implement efficient database queries and indexing.

**Phase 6: Testing**

-   **Unit and Integration Testing**:
    -   Test each component individually.
    -   Ensure the ETL process correctly populates the database.

-   **User Acceptance Testing (UAT)**:
    -   Allow a group of users to test the new system.
    -   Gather feedback and make necessary adjustments.

**Phase 7: Deployment**

-   **Infrastructure Setup**:
    -   Configure GCP services for production.
    -   Set up monitoring and logging tools.

-   **Deploy Applications**:
    -   Use CI/CD pipelines for consistent deployments.
    -   Roll out changes during low-traffic periods to minimize impact.

**Phase 8: Monitoring and Maintenance**

-   **Monitor Performance**:
    -   Use GCP monitoring tools to track application health.
    -   Set up alerts for critical issues.

-   **Continuous Improvement**:
    -   Regularly update dependencies and security patches.
    -   Schedule periodic reviews of system performance and architecture.

## Advantages of the Proposed Architecture

-   **Improved Performance**:
    -   Eliminating real-time data processing reduces load times.
    -   Pre-processed data allows for faster analytics and reporting.

-   **Simplified Maintenance**:
    -   Fewer moving parts make the system easier to manage.
    -   Unified codebases reduce complexity.

-   **Enhanced Security**:
    -   Managed authentication services provide robust security features.
    -   Centralized data storage simplifies compliance efforts.

-   **Cost Efficiency**:
    -   Consolidated services reduce infrastructure costs.
    -   Optimized resource utilization on GCP.

-   **Scalability**:
    -   GCP services and container orchestration support scaling as user demand grows.

## Potential Challenges and Mitigation Strategies

-   **Data Migration Risks**:
    -   **Mitigation**: Perform migrations during off-peak hours and keep backups.
-   **Learning Curve with New Services**:
    -   **Mitigation**: Provide training sessions for the team on new tools and services.
-   **User Impact During Transition**:
    -   **Mitigation**: Communicate changes to users in advance and offer support.
-   **Dependency Management**:
    -   **Mitigation**: Use package management tools and maintain a clear documentation of dependencies.

## Next Steps

-   **Stakeholder Buy-In**:
    -   Present this plan to stakeholders for approval.
    -   Adjust based on feedback and priorities.
-   **Resource Allocation**:
    -   Assign team members to specific tasks.
    -   Ensure the team has access to necessary tools and training.
-   **Timeline Establishment**:
    -   Set realistic deadlines for each phase.
    -   Incorporate buffer time for unexpected issues.

## Conclusion

By restructuring your system architecture, you will achieve a more maintainable, efficient, and scalable solution. Consolidating databases and services simplifies the ecosystem, while implementing an ETL process enhances performance and data integrity. Simplifying authentication reduces security risks and maintenance overhead.

