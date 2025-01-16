# Styles (SASS/SCSS)

> [!NOTE]
> This folder contains the styles for the application.
> The styles are written in SASS/SCSS and are compiled to CSS.
> The compiled CSS is then included in the application.

## Structure

- **index.scss**: ([index.scss](./index.scss)) - This file is the entry point for the styles. It imports all other style files and compiles them into a single CSS file. This file should not contain any styles but should only import other style files.

- **Elements**: ([elements/](./elements/)) - This folder contains reusable UI elements or small components that are often used in multiple places across the app (e.g., buttons, tables, cards). These elements do not define the layout but are self-contained elements styled independently.

- **Layout**: ([layout/](./layout/)) - This folder groups styles related to the structural components of the app, such as the footer, navbar, and sidebar. These styles influence the app's primary structure and page layout, often containing nested elements.

- **Theme**: ([theme/](./theme/)) - This folder defines all theming aspects, with files for colors, typography, company-specific branding, logos, and social icons. By centralizing theme-related files here, you create a clear and cohesive styling foundation that can be easily adapted to different branding requirements.

- **Utility**: ([utility/](./utility/)) - This folder holds helper styles, mixins, and global settings like maps styling (_gmaps.scss), hover effects, utility classes, and general settings. This folder is ideal for styles that apply globally or offer helpers to other style files.

## Importing in `index.scss`

To compile everything, you can import these in index.scss in a structured order.

A good order for imports would be:

- **Utilities**: Importing utilities first is logical since variables, mixins, and helper styles are often referenced throughout other styles.
- **Theme**: Import theme variables, typography, and color settings next, since these are foundational and might be used across elements and layout styles.
- **Layout**: Import layout components next to establish the main structure of the app.
- **Elements**: Finally, import elements to complete the styling of reusable UI components.

```scss
// Import utilities
@import "utility/utils";
@import "utility/settings";
@import "utility/hover";
@import "utility/gmaps";

// Import theme
@import "theme/colors";
@import "theme/company";
@import "theme/logos";
@import "theme/social";
@import "theme/typography";

// Import layout
@import "layout/footer";
@import "layout/navbar";
@import "layout/page";
@import "layout/sidebar";
@import "layout/tabs";

// Import elements
@import "elements/buttons";
@import "elements/cards";
@import "elements/selectize";
@import "elements/tables";
```

## Structure Benefits

- **Consistency**: This setup keeps all files neatly organized and aligned by functionality, making it clear where to add new styles or modify existing ones.

- **Scalability**: As the project grows, you can easily add new files to each section without disturbing other parts of the app.

- **Theming and Utilities**: Keeping theming and utilities separate makes updating styles across the app straightforward and consistent, especially if branding requirements change.

This structure is robust for a scalable Shiny app or any larger project that requires clear, maintainable SCSS organization.
