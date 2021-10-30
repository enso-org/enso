/**
 * Scrollbar style definitions for textual visualizations which need support for scrolling.
 *
 * The 11px width/height (depending on scrollbar orientation)
 * is set so that it resembles macOS default scrollbar.
 */

const scrollbarStyle = `
    .scrollable::-webkit-scrollbar { -webkit-appearance: none; }
    
    .scrollable::-webkit-scrollbar-track { -webkit-box-shadow: none; }
    
    .scrollable::-webkit-scrollbar:vertical { width: 11px; }
    
    .scrollable::-webkit-scrollbar:horizontal { height: 11px; }
    
    .scrollable::-webkit-scrollbar-thumb {
        border-radius: 8px;
        border: 1px solid rgba(220, 220, 220, .5);
        background-color: rgba(190, 190, 190, .5);
    }
    
    .scrollable::-webkit-scrollbar-corner { background: rgba(0,0,0,0); }
`
