class LazyScroll {
    constructor() {
        this.settings = null;
        // Holds the last scrop position to detect scroll down or up
        this.lastScrollTop = 0;
        //contains cache of rendered list
        this.rowElems;
    }

    // Initialization methods
    init(settings) {
        this.settings = settings;

        this.destroyGrid();
        this.initializeProperties();
        this.initializeEvents();
        this.handleScroll();
    }

    //Initialize setup like setting up the total height of the list and finding out the
    // total number of visible items based on the allowed height.
    initializeProperties() {
        let totalHeight = this.settings.dataLength * this.settings.rowHeight;

        this.settings.parentElem.style.height = totalHeight + 'px';
        this.settings.parentElem.style.position = 'relative';
        this.settings.fixedHeightContainerElem.scrollTop = 0;
        this.settings.parentOffsetTop = this.settings.parentElem.getBoundingClientRect().top;

        let fixedContainerElemHeight = this.settings.fixedHeightContainerElem
            .clientHeight;
        this.settings.visibleRowsCount = Math.ceil(
            fixedContainerElemHeight / this.settings.rowHeight
        );

        let temp = Math.max(this.settings.visibleRowsCount, 20);
        //this.settings.extraVisibleRowCount = temp + (temp % 2); // Make it even
        this.settings.extraVisibleRowCount = temp * 10; // Make it even
        this.isScrollDown = true;
        this.requestedOffset = 0;
        this.requestedLimit = 0;
    }

    //get the bounding values of the main element which is of the fixed height.
    getParentClientRect() {
        this.settings.parentClientRect = this.settings.fixedHeightContainerElem.getBoundingClientRect();
        return this.settings.parentClientRect;
    }

    //Removing & Attaching Scroll events.
    initializeEvents() {
        let listener = (evt) => {
            this.handleScrollEvent(evt);
        };
        this.settings.fixedHeightContainerElem.removeEventListener(
            'scroll',
            listener
        );
        this.settings.fixedHeightContainerElem.addEventListener(
            'scroll',
            listener
        );
    }

    handleScrollEvent1(evt) {
        if (requestAnimationFrame) {
            console.log('requestAnimationFrame');
            requestAnimationFrame(() => {
                this.handleScrollEvent1(evt);
            });
        } else {
            console.log('NOT requestAnimationFrame');
            this.handleScrollEvent1(evt);
        }
    }

    // handleScrollEvent determines whether user is scrolling down or up.
    // it stores the last scroll position in scrollTop & compares with current.
    // if current scroll top is greater than last then user is scrolling down
    // else scroll up
    handleScrollEvent(evt) {
        var scrollTop = this.settings.fixedHeightContainerElem.scrollTop;
        console.log('handleScrollEvent', scrollTop, this.lastScrollTop);
        if (scrollTop !== this.lastScrollTop) {
            let func,
                isScrollDown = scrollTop > this.lastScrollTop;
            if (isScrollDown) {
                func = this.handleScrollDown;
            } else {
                func = this.handleScrollUp;
            }
            this.lastScrollTop = scrollTop;
            func.call(this, evt);
        } else {
            console.log(
                'horizontal scroll detected. Currently its not handled for virtual scroll'
            );
        }
    }

    /**
     *
     * when user is scrolling down,
     * 1. calculates the next set of row element to be rendered. Look at the last row element rendered in the list and get its the position
     * 2. Find the next range by adding the number of visible rows to the end position
     * 3. Renders the new range
     **/
    handleScrollDown(evt) {
        if (this.rowElems && this.rowElems.length > 0) {
            //console.log('rowElems', this.rowElems);
            let lastChildElem = this.rowElems[this.rowElems.length - 1];
            let lastIndexNumber = Number(lastChildElem.dataset.indexnumber);
            lastIndexNumber = isNaN(lastIndexNumber) ? 0 : lastIndexNumber;
            lastIndexNumber = this.requestedOffset + this.requestedLimit;
            let clientRect = lastChildElem.getBoundingClientRect();
            console.log('handleScrollDown', lastChildElem, clientRect);
            if (clientRect.bottom < 0) {
                //Check if all the elems are rendered.
                if (lastIndexNumber + 1 < this.settings.dataLength) {
                    //Last element went up and page is empty. Need to calculate the page number now
                    this.handleScroll(evt, true);
                }
            } else if (
                clientRect.bottom <
                this.getParentClientRect().bottom + 5 * this.settings.rowHeight
            ) {
                //Last element went up and element is still in the page. Can do continuous rendering
                let startCount =
                    parseInt(
                        lastChildElem.getAttribute('data-indexnumber'),
                        10
                    ) + 1;
                //startCount = this.requestedOffset;
                let endCount = startCount + this.settings.extraVisibleRowCount;
                this.isScrollDown = true;
                console.log('we are here');
                this.renderPageData(startCount, endCount, true);
            }
        } else {
            this.handleScroll(evt, true);
        }
    }

    /**
     *
     * when user is scrolling and unable to determine the range then,
     * 1. find the range by finding the scroll users has done.
     * 2. divide the scroll value by the height of each row element.
     * 3. division will give the current page count. Caculate the range based on the current page
     * 4. Render the current range.
     *
     * This is usefull when user is scrolling very fast. This helps from jumping one page to any other page.
     **/
    handleScroll(evt, isScrollDown) {
        console.log('handleScroll', isScrollDown);
        let currentStartIndex = Math.floor(
            this.settings.fixedHeightContainerElem.scrollTop /
                this.settings.rowHeight
        );
        let start = currentStartIndex - this.settings.extraVisibleRowCount / 2;
        let end =
            currentStartIndex +
            this.settings.visibleRowsCount +
            this.settings.extraVisibleRowCount / 2;

        this.isScrollDown = true;
        let childs = this.settings.rowParentElem.querySelectorAll(
            this.settings.rowSelector
        );
        let noOfChildPresent = childs.length;
        let counter = noOfChildPresent - 1;
        let upperCount = 0;
        this.removeAllChilds(
            counter,
            upperCount,
            this.settings.rowParentElem,
            childs
        );
        this.renderPageData(start, end, isScrollDown, true);
    }

    /**
     * when user is scrolling up, exact opposite of scroll down
     *
     * 1. find the position of first row element present in the list
     * 2. calculate the range by subracting from the visible number of row elements of the list
     * 3. Render new range.
     *
     * **/
    handleScrollUp(evt) {
        if (this.rowElems && this.rowElems.length > 0) {
            let firstChildElem = this.rowElems[0];
            let clientRect = firstChildElem.getBoundingClientRect();
            if (clientRect.top > this.getParentClientRect().bottom) {
                this.handleScroll(evt, false);
            } else if (
                clientRect.top >
                this.getParentClientRect().top - 5 * this.settings.rowHeight
            ) {
                let endCount = parseInt(
                    firstChildElem.getAttribute('data-indexNumber'),
                    10
                );
                let startCount = endCount - this.settings.extraVisibleRowCount;
                this.isScrollDown = false;
                this.renderPageData(startCount, endCount, false);
            }
        } else {
            this.handleScroll(evt, false);
        }
    }

    /**
     * it removes the row element from the parent list. Utility method.
     * */
    removeAllChilds(upperCount, lowerCounter, childsParent, childs) {
        console.log('removeAllChilds', upperCount, lowerCounter);
        for (; upperCount >= lowerCounter; upperCount--) {
            if (childsParent.length === 0) {
                console.error('error here');
            }
            if (childs[upperCount]) {
                childsParent.removeChild(childs[upperCount]);
            }
        }
        this.rowElems = this.settings.rowParentElem.querySelectorAll(
            this.settings.rowSelector
        );
    }

    requestData(obj) {
        this.settings.requestDataFn({
            offset: obj.startCount,
            limit: obj.length,
        });
    }

    /**
     *
     * renderPageData :- the main logic of the service.
     *
     * Two main task
     * 1. It renders the given range on the screen
     * 2. it removes the row elements which are not visible in the current view port.
     *
     * **/
    renderPageData(startCount, endCount, isScrollDown, removeAllChild) {
        console.log('renderPageData', startCount, endCount);
        let rowParentElem = this.settings.rowParentElem;
        //Check to avoid negative indexing
        if (startCount < 0) {
            startCount = 0;
        }

        let totalDataLength = this.settings.dataLength;

        if (startCount >= totalDataLength) {
            if (totalDataLength === 0) {
                //if collection becomes empty, then remove all the existing list rows
                let childs = rowParentElem.querySelectorAll(
                    settings.rowSelector
                );
                this.removeAllChilds(
                    childs.length - 1,
                    0,
                    rowParentElem,
                    childs
                );
            }
            console.log('returning as no more data to render');
            return;
        }

        //check to avoid wrong indexing
        if (endCount > totalDataLength) {
            endCount = totalDataLength;
        } else if (endCount <= 0) {
            return;
        }

        let noOfElems = endCount - startCount;
        // let childs = rowParentElem.querySelectorAll(this.settings.rowSelector);

        if (
            this.requestedOffset === startCount &&
            this.requestedLimit === noOfElems
        ) {
            return;
        }
        this.settings.requestDataFn({
            offset: startCount,
            limit: noOfElems,
        });
        this.requestedOffset = startCount;
        this.requestedLimit = noOfElems;
    }

    renderFragment(fragment) {
        console.log('renderFragment');

        let noOfElems = fragment.children.length;
        let rowParentElem = this.settings.rowParentElem;
        let childs = rowParentElem.querySelectorAll(this.settings.rowSelector);
        let isScrollDown = this.isScrollDown;

        let counter = 0,
            upperCount = 1,
            noOfChildPresent = childs.length;
        // if (removeAllChild) {
        //     counter = noOfChildPresent - 1;
        //     upperCount = 0;
        //     //remove the elements from the dom tree.
        //     this.removeAllChilds(counter, upperCount, rowParentElem, childs);
        // } else
        if (
            noOfChildPresent >
            this.settings.visibleRowsCount + this.settings.extraVisibleRowCount
        ) {
            if (isScrollDown) {
                counter = Math.min(noOfElems, noOfChildPresent);
                counter--;
                upperCount = 0;
            } else {
                counter = noOfChildPresent - 1;
                upperCount = counter - noOfElems;
            }
        }
        childs = null;

        if (isScrollDown) {
            this.settings.rowParentElem.appendChild(fragment);
        } else {
            this.settings.rowParentElem.insertBefore(
                fragment,
                this.settings.rowParentElem.children[0]
            );
        }

        this.rowElems = rowParentElem.querySelectorAll(
            this.settings.rowSelector
        );
        if (this.rowElems.length === 0) {
            console.error('Rendering error');
        } else {
            let extraChildCount, invisibleRowsCount, invisibleRowsHeight;
            if (isScrollDown) {
                let firstElem = this.rowElems[0];
                invisibleRowsHeight =
                    firstElem.getBoundingClientRect().top -
                    this.getParentClientRect().top;
                if (invisibleRowsHeight < 0) {
                    invisibleRowsCount = Math.floor(
                        Math.abs(invisibleRowsHeight) / this.settings.rowHeight
                    );
                    extraChildCount =
                        invisibleRowsCount - this.settings.extraVisibleRowCount;
                    if (extraChildCount > 0) {
                        //remove the elements from the dom tree.
                        this.removeAllChilds(
                            extraChildCount,
                            0,
                            rowParentElem,
                            this.rowElems
                        );
                        this.rowElems = rowParentElem.querySelectorAll(
                            this.settings.rowSelector
                        );
                    }
                }
            } else {
                let lastElem = this.rowElems[this.rowElems.length - 1];
                invisibleRowsHeight =
                    lastElem.getBoundingClientRect().bottom -
                    this.getParentClientRect().bottom;
                if (invisibleRowsHeight > 0) {
                    invisibleRowsCount = Math.floor(
                        invisibleRowsHeight / this.settings.rowHeight
                    );
                    extraChildCount =
                        invisibleRowsCount - this.settings.extraVisibleRowCount;
                    if (extraChildCount > 0) {
                        this.removeAllChilds(
                            this.rowElems.length - 1,
                            this.rowElems.length - extraChildCount - 1,
                            rowParentElem,
                            this.rowElems
                        );
                        this.rowElems = rowParentElem.querySelectorAll(
                            this.settings.rowSelector
                        );
                    }
                }
            }
        }
    }

    destroyGrid() {
        console.log('destroyGrid', this.settings);
        this.settings.fixedHeightContainerElem &&
            this.settings.fixedHeightContainerElem.removeEventListener(
                'scroll',
                this.handleScrollEvent
            );
    }
}
