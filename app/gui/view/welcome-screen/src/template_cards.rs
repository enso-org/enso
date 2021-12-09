//! Template cards for Welcome Screen.
//!
//! Template cards allow user to select a predefined template by clicking on the corresponding
//! card.

use ensogl::prelude::*;

use crate::ClickableElement;

use enso_frp as frp;
use ensogl::system::web;
use ensogl::system::web::AttributeSetter;
use ensogl::system::web::NodeInserter;
use wasm_bindgen::JsCast;
use web_sys::Element;
use web_sys::HtmlDivElement;



// ======================
// === Template Cards ===
// ======================


// === CardDefinition struct. ===

struct CardDefinition {
    class:                &'static str,
    background_image_url: Option<&'static str>,
    header:               &'static str,
    content:              &'static str,
    template:             &'static str,
}


// === Predefined cards. ===

const CARD_SPREADSHEETS: CardDefinition = CardDefinition {
    class:                crate::css_class::CARD_SPREADSHEETS,
    background_image_url: Some("/assets/spreadsheets.png"),
    header:               "Combine spreadsheets",
    content:              "Glue multiple spreadsheets together to analyse all your data at once.",
    template:             "orders",
};
const CARD_GEO: CardDefinition = CardDefinition {
    class:                crate::css_class::CARD_GEO,
    background_image_url: None,
    header:               "Geospatial analysis",
    content:              "Learn where to open a coffee shop to maximize your income.",
    template:             "restaurants",
};
const CARD_VISUALIZE: CardDefinition = CardDefinition {
    class:                crate::css_class::CARD_VISUALIZE,
    background_image_url: None,
    header:               "Analyze GitHub stars",
    content:              "Find out which of Enso's repositories are most popular over time.",
    template:             "stargazers",
};


// === Card struct. ===

#[derive(Debug, Clone)]
struct Card {
    pub clickable_element: ClickableElement,
    pub template_name:     &'static str,
}

impl Deref for Card {
    type Target = ClickableElement;
    fn deref(&self) -> &Self::Target {
        &self.clickable_element
    }
}


// =============
// === Model ===
// =============

#[derive(Debug, Clone, CloneRef)]
pub struct Model {
    logger:       Logger,
    pub root_dom: Element,
    cards:        Rc<Vec<Card>>,
}

impl Model {
    /// Constructor.
    pub fn new(logger: Logger, open_template: &frp::Any<String>) -> Self {
        let root_dom = web::create_element("main");
        root_dom.set_class_name(crate::css_class::CONTENT);
        let templates = web::create_div();

        let header = Self::create_header("Templates");
        templates.append_or_warn(&header, &logger);

        let (cards_dom, cards) = Self::create_cards(&logger);
        templates.append_or_warn(&cards_dom, &logger);
        root_dom.append_or_warn(&templates, &logger);

        let model = Self { logger, root_dom, cards: Rc::new(cards) };
        model.setup_event_listeners(open_template);
        model
    }

    /// Attach click event for every card to `open_template` FRP endpoint.
    fn setup_event_listeners(&self, open_template: &frp::Any<String>) {
        for card in self.cards.iter() {
            let network = &card.network;
            let template_name = card.template_name.to_owned();
            frp::extend! { network
                open_template <+ card.click.constant(template_name);
            }
        }
    }

    fn create_header(content: &str) -> Element {
        let header = web::create_element("h2");
        header.set_text_content(Some(content));
        header
    }

    /// Create main content, a set of cards.
    fn create_cards(logger: &Logger) -> (HtmlDivElement, Vec<Card>) {
        let mut cards = Vec::new();
        let dom = web::create_div();
        dom.set_class_name(crate::css_class::CARDS);

        let row1 = Self::create_row(&[CARD_SPREADSHEETS, CARD_GEO], &mut cards, logger);
        dom.append_or_warn(&row1, logger);

        let row2 = Self::create_row(&[CARD_VISUALIZE], &mut cards, logger);
        dom.append_or_warn(&row2, logger);

        (dom, cards)
    }

    fn create_row(
        definitions: &[CardDefinition],
        cards: &mut Vec<Card>,
        logger: &Logger,
    ) -> HtmlDivElement {
        let row = web::create_div();
        row.set_class_name(crate::css_class::ROW);
        for definition in definitions {
            let card = Self::create_card(definition, logger);
            row.append_or_warn(&card.element, logger);
            cards.push(card.clone());
        }
        row
    }

    /// Helper to create a single card DOM from provided definition.
    fn create_card(definition: &CardDefinition, logger: &Logger) -> Card {
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

        let clickable_element = ClickableElement::new(card.unchecked_into(), logger);
        Card { clickable_element, template_name: definition.template }
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {}
    Output {
        // Create a new project from template `name`.
        open_template(String),
    }
}



// =====================
// === TemplateCards ===
// =====================

/// Template Cards for Welcome View. It contains a few predefined template cards. Clicking on
/// a template card creates a new project with some prepared code.
#[derive(Debug, Clone, CloneRef)]
pub struct TemplateCards {
    pub model: Model,
    pub frp:   Frp,
}

impl Deref for TemplateCards {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl TemplateCards {
    pub fn new(logger: &Logger) -> Self {
        let logger = Logger::new_sub(logger, "TemplateCards");
        let frp = Frp::new();
        let model = Model::new(logger, &frp.output.source.open_template);
        Self { model, frp }
    }
}
