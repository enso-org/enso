//! Template cards for Welcome Screen.
//!
//! Template cards allow user to select a predefined template by clicking on the corresponding
//! card.

use ensogl::prelude::*;

use crate::ClickClosure;

use ensogl::system::web;
use ensogl::system::web::AttributeSetter;
use ensogl::system::web::NodeInserter;
use web_sys::Element;
use web_sys::HtmlDivElement;



// ========================
// === Cards Definition ===
// ========================


// === Card struct. ===

struct Card<'a> {
    class:                &'a str,
    background_image_url: Option<&'a str>,
    header:               &'a str,
    content:              &'a str,
}


// === Predefined cards. ===

const CARD_SPREADSHEETS: Card<'static> = Card {
    class:                crate::css_class::CARD_SPREADSHEETS,
    background_image_url: Some("/assets/spreadsheets.png"),
    header:               "Combine spreadsheets",
    content:              "Glue multiple spreadsheets together to analyse all your data at once.",
};
const CARD_GEO: Card<'static> = Card {
    class:                crate::css_class::CARD_GEO,
    background_image_url: None,
    header:               "Geospatial analysis",
    content:              "Learn where to open a coffee shop to maximize your income.",
};
const CARD_VISUALIZE: Card<'static> = Card {
    class:                crate::css_class::CARD_VISUALIZE,
    background_image_url: None,
    header:               "Analyze GitHub stars",
    content:              "Find out which of Enso's repositories are most popular over time.",
};



// ======================
// === Template Cards ===
// ======================

/// Template Cards for Welcome View. It contains a few predefined template cards. Clicking on
/// a template card creates a new project with some prepared code.
#[derive(Debug, Clone, CloneRef)]
pub struct TemplateCards {
    logger:       Logger,
    pub root_dom: Element,
    closures:     Rc<RefCell<Vec<ClickClosure>>>,
}

impl TemplateCards {
    /// Constructor.
    pub fn new(logger: &Logger) -> Self {
        let logger = Logger::new_sub(logger, "TemplateCards");
        let root_dom = web::create_element("main");
        root_dom.set_class_name(crate::css_class::CONTENT);

        let templates = Self::create_templates(&logger);
        root_dom.append_or_warn(&templates, &logger);

        Self { logger, root_dom, closures: default() }
    }

    fn create_templates(logger: &Logger) -> HtmlDivElement {
        let templates = web::create_div();

        let header = Self::create_header("Templates");
        templates.append_or_warn(&header, logger);

        let cards = Self::create_cards(logger);
        templates.append_or_warn(&cards, logger);

        templates
    }

    fn create_header(content: &str) -> Element {
        let header = web::create_element("h2");
        header.set_text_content(Some(content));
        header
    }

    /// Create main content, a set of cards.
    fn create_cards(logger: &Logger) -> HtmlDivElement {
        let cards = web::create_div();
        cards.set_class_name(crate::css_class::CARDS);

        let row1 = Self::create_row(logger, &[CARD_SPREADSHEETS, CARD_GEO]);
        cards.append_or_warn(&row1, logger);

        let row2 = Self::create_row(logger, &[CARD_VISUALIZE]);
        cards.append_or_warn(&row2, logger);

        cards
    }

    fn create_row(logger: &Logger, cards: &[Card<'_>]) -> HtmlDivElement {
        let row = web::create_div();
        row.set_class_name(crate::css_class::ROW);
        for definition in cards {
            let card = Self::create_card(logger, definition);
            row.append_or_warn(&card, logger);
        }
        row
    }

    /// Helper to create a single card DOM from provided definition.
    fn create_card(logger: &Logger, definition: &Card<'_>) -> HtmlDivElement {
        let card = web::create_div();
        card.set_class_name(&format!("{} {}", crate::css_class::CARD, definition.class));
        if let Some(src) = definition.background_image_url {
            let img = web::create_element("img");
            img.set_attribute_or_warn("src", src, logger);
            card.append_or_warn(&img, logger);
        }
        let card_header = web::create_element("h3");
        card_header.set_text_content(Some(definition.header));
        card.append_or_warn(&card_header, logger);
        let text_content = web::create_element("p");
        text_content.set_text_content(Some(definition.content));
        card.append_or_warn(&text_content, logger);

        card
    }
}
