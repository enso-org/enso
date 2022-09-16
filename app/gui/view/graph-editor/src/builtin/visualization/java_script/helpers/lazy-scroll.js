class LazyScroll {
    constructor() {
        this.settings = null;
        // Holds the last scrop position to detect scroll down or up
        this.lastScrollTop = 0;
    }

    // Initialization methods
    init(settings) {
        this.settings = settings;

        this.initializeProperties();
        this.initializeEvents();
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

        this.settings.extraVisibleRowCount =
            this.settings.visibleRowsCount * 50;
        this.settings.fetchLimitRowsCount =
            this.settings.extraVisibleRowCount / 2;
        this.settings.fetchRequestDelay = 25;
    }

    //get the bounding values of the main element which is of the fixed height.
    getParentClientRect() {
        this.settings.parentClientRect = this.settings.fixedHeightContainerElem.getBoundingClientRect();
        return this.settings.parentClientRect;
    }

    // Initialize event listener
    initializeEvents() {
        this.settings.fixedHeightContainerElem.addEventListener(
            'scroll',
            (evt) => {
                this.handleScrollEvent(evt);
            }
        );
    }

    handleScrollEvent(evt) {
        let scrollTop = this.settings.fixedHeightContainerElem.scrollTop;
        // console.log('handleScrollEvent', scrollTop, this.lastScrollTop);
        if (scrollTop !== this.lastScrollTop) {
            let func;
            let isScrollDown = scrollTop > this.lastScrollTop;
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

    handleScrollDown(evt) {
        let lastChildElem = this.settings.rowParentElem.lastChild;
        if (lastChildElem) {
            let lastIndexNumber = Number(lastChildElem.dataset.indexnumber);
            let clientRect = lastChildElem.getBoundingClientRect();
            if (clientRect.bottom < 0) {
                //Last element went up and page is empty. Need to calculate the page number now
                this.handleScroll(evt);
            } else if (
                clientRect.bottom <
                this.getParentClientRect().bottom +
                    this.settings.fetchLimitRowsCount * this.settings.rowHeight
            ) {
                //Last element went up and element is still in the page. Can do continuous rendering
                let start = Number(lastChildElem.dataset.indexnumber) + 1;
                let end = start + this.settings.extraVisibleRowCount;
                this.requestPageData(start, end);
            }
        } else {
            this.handleScroll(evt);
        }
    }

    handleScrollUp(evt) {
        let firstChildElem = this.settings.rowParentElem.firstChild;
        if (firstChildElem) {
            let clientRect = firstChildElem.getBoundingClientRect();
            if (clientRect.top > this.getParentClientRect().bottom) {
                this.handleScroll(evt);
            } else if (
                clientRect.top >
                this.getParentClientRect().top -
                    this.settings.fetchLimitRowsCount * this.settings.rowHeight
            ) {
                let endCount = Number(firstChildElem.dataset.indexnumber) - 1;
                let startCount = endCount - this.settings.extraVisibleRowCount;
                this.requestPageData(startCount, endCount);
            }
        } else {
            this.handleScroll(evt);
        }
    }

    handleScroll(evt) {
        console.log('handleScroll');
        let currentStartIndex = Math.floor(
            this.settings.fixedHeightContainerElem.scrollTop /
                this.settings.rowHeight
        );
        let start = currentStartIndex - this.settings.extraVisibleRowCount;
        let end =
            currentStartIndex +
            this.settings.visibleRowsCount +
            this.settings.extraVisibleRowCount;
        this.removeAllRows();
        this.requestPageData(start, end);
    }

    removeAllRows() {
        let rowParentElem = this.settings.rowParentElem;
        console.log('removeAllRows');
        let firstElementChild = rowParentElem.firstElementChild;
        while (firstElementChild) {
            firstElementChild.remove();
            firstElementChild = rowParentElem.firstElementChild;
        }
    }

    requestPageData(startIndex, endIndex) {
        //Check to avoid negative indexing
        if (startIndex < 0) {
            startIndex = 0;
        }

        let totalDataLength = this.settings.dataLength;

        if (startIndex > totalDataLength) {
            return;
        }

        if (endIndex > totalDataLength) {
            endIndex = totalDataLength;
        }

        if (endIndex <= startIndex) {
            return;
        }

        if (
            this.requestedStartIndex === startIndex ||
            this.requestedEndIndex === endIndex
        ) {
            return;
        }

        console.log('requestPageData', startIndex, endIndex);

        clearTimeout(this.timeoutId);
        this.timeoutId = setTimeout(() => {
            this.settings.requestDataFn(startIndex, endIndex);
        }, this.settings.fetchRequestDelay);

        this.requestedStartIndex = startIndex;
        this.requestedEndIndex = endIndex;
    }

    renderFragment(fragment) {
        // Check if ab overlaps cd.
        function isOverlap(a, b, c, d) {
            return (a >= c && a <= d) || (b >= c && b <= d);
        }

        let rowParentElem = this.settings.rowParentElem;

        let fragmentStart = Number(
            fragment.firstElementChild.dataset.indexnumber
        );
        let fragmentEnd = Number(fragment.lastElementChild.dataset.indexnumber);
        let childsStart = -1;
        let childsEnd = -1;
        if (rowParentElem.lastChild) {
            childsStart = Number(rowParentElem.firstChild.dataset.indexnumber);
            childsEnd = Number(rowParentElem.lastChild.dataset.indexnumber);
        }

        // cleanup if fragment intersects with childs
        if (
            isOverlap(fragmentStart, fragmentEnd, childsStart, childsEnd) ||
            isOverlap(childsStart, childsEnd, fragmentStart, fragmentEnd)
        ) {
            this.removeAllRows();
        }

        console.log(
            'renderFragment',
            [fragmentStart, fragmentEnd],
            [childsStart, childsEnd]
        );

        if (fragmentStart > childsEnd) {
            if (rowParentElem.firstChild) {
                let invisibleTopHeight = Math.abs(
                    rowParentElem.firstChild.getBoundingClientRect().top -
                        this.getParentClientRect().top
                );
                let invisibleTopRows = Math.floor(
                    invisibleTopHeight / this.settings.rowHeight
                );
                let extraTopRows =
                    invisibleTopRows - this.settings.extraVisibleRowCount;

                console.log('extraTopRows', extraTopRows);
                let firstChild = rowParentElem.firstChild;
                while (extraTopRows > 0 && firstChild) {
                    extraTopRows -= 1;
                    firstChild.remove();
                    firstChild = rowParentElem.firstChild;
                }
            }

            this.settings.rowParentElem.appendChild(fragment);
        } else {
            if (rowParentElem.lastChild) {
                let invisibleBottomHeight =
                    rowParentElem.lastChild.getBoundingClientRect().bottom -
                    this.getParentClientRect().bottom;
                let invisibleBottomRows = Math.floor(
                    invisibleBottomHeight / this.settings.rowHeight
                );
                let extraBottomRows =
                    invisibleBottomRows - this.settings.extraVisibleRowCount;

                console.log('extraBottomRows', extraBottomRows);

                let lastChild = rowParentElem.lastChild;
                while (extraBottomRows > 0 && lastChild) {
                    extraBottomRows -= 1;
                    lastChild.remove();
                    lastChild = rowParentElem.lastChild;
                }
            }

            rowParentElem.insertBefore(fragment, rowParentElem.firstChild);
        }
    }
}
