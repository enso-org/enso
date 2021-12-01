//! Template cards for Welcome Screen.
//!
//! Template cards allow user to select a predefined template by clicking on the corresponding
//! card.

use ensogl::prelude::*;

use crate::ClickClosure;
use crate::Frp;

use ensogl::system::web;
use ensogl::system::web::AttributeSetter;
use ensogl::system::web::NodeInserter;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::Element;
use web_sys::HtmlDivElement;
use web_sys::MouseEvent;



// ========================
// === Cards Definition ===
// ========================


// === Card struct. ===

struct Card<'a> {
    class:                &'a str,
    background_image_url: Option<&'a str>,
    header:               &'a str,
    content:              &'a str,
    template:             &'a str,
}


// === Predefined cards. ===

const CARD_SPREADSHEETS: Card<'static> = Card {
    class:                crate::css_class::CARD_SPREADSHEETS,
    background_image_url: Some("/assets/spreadsheets.png"),
    header:               "Combine spreadsheets",
    content:              "Glue multiple spreadsheets together to analyse all your data at once.",
    template:             "orders",
};
const CARD_GEO: Card<'static> = Card {
    class:                crate::css_class::CARD_GEO,
    background_image_url: None,
    header:               "Geospatial analysis",
    content:              "Learn where to open a coffee shop to maximize your income.",
    template:             "restaurants",
};
const CARD_VISUALIZE: Card<'static> = Card {
    class:                crate::css_class::CARD_VISUALIZE,
    background_image_url: None,
    header:               "Analyze GitHub stars",
    content:              "Find out which of Enso's repositories are most popular over time.",
    template:             "stargazers",
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
    frp:          Frp,
    closures:     Rc<RefCell<Vec<ClickClosure>>>,
}

impl TemplateCards {
    /// Constructor.
    pub fn new(logger: &Logger, frp: Frp) -> Self {
        let logger = Logger::new_sub(logger, "TemplateCards");
        let root_dom = web::create_element("main");
        root_dom.set_class_name(crate::css_class::CONTENT);

        Self { logger, frp, root_dom, closures: default() }.init()
    }

    fn init(self) -> Self {
        let templates = web::create_div();

        let header = Self::create_header("Templates");
        templates.append_or_warn(&header, &self.logger);

        let cards = self.create_cards();
        templates.append_or_warn(&cards, &self.logger);

        self.root_dom.append_or_warn(&templates, &self.logger);

        self
    }

    fn create_header(content: &str) -> Element {
        let header = web::create_element("h2");
        header.set_text_content(Some(content));
        header
    }

    /// Create main content, a set of cards.
    fn create_cards(&self) -> HtmlDivElement {
        let cards = web::create_div();
        cards.set_class_name(crate::css_class::CARDS);

        let row1 = self.create_row(&[CARD_SPREADSHEETS, CARD_GEO]);
        cards.append_or_warn(&row1, &self.logger);

        let row2 = self.create_row(&[CARD_VISUALIZE]);
        cards.append_or_warn(&row2, &self.logger);

        cards
    }

    fn create_row(&self, cards: &[Card<'_>]) -> HtmlDivElement {
        let row = web::create_div();
        row.set_class_name(crate::css_class::ROW);
        for definition in cards {
            let card = self.create_card(definition);
            self.setup_card_event_listener(&card, definition.template);
            row.append_or_warn(&card, &self.logger);
        }
        row
    }

    /// Helper to create a single card DOM from provided definition.
    fn create_card(&self, definition: &Card<'_>) -> HtmlDivElement {
        let card = web::create_div();
        card.set_class_name(&format!("{} {}", crate::css_class::CARD, definition.class));
        if let Some(src) = definition.background_image_url {
            let img = web::create_element("img");
            img.set_attribute_or_warn("src", src, &self.logger);
            card.append_or_warn(&img, &self.logger);
        }
        let card_header = web::create_element("h3");
        card_header.set_text_content(Some(definition.header));
        card.append_or_warn(&card_header, &self.logger);
        let text_content = web::create_element("p");
        text_content.set_text_content(Some(definition.content));
        card.append_or_warn(&text_content, &self.logger);

        card
    }

    fn setup_card_event_listener(&self, card: &Element, template_name: &str) {
        let create_project = self.frp.create_project.clone_ref();
        let template = Some(template_name.to_owned());
        let closure = Box::new(move |_event: MouseEvent| {
            create_project.emit(template.clone());
        });
        let closure: ClickClosure = Closure::wrap(closure);
        let callback = closure.as_ref().unchecked_ref();
        if card.add_event_listener_with_callback("click", callback).is_err() {
            error!(self.logger, "Could not add open template event listener for {template_name}.");
        }
        self.closures.borrow_mut().push(closure);
    }
}
