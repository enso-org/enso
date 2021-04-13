class ImageBase64Visualization extends Visualization {
    static inputType = ' Standard.Image.Data.Image.Image'
    static label = 'Image'

    onDataReceived(data) {
        function image(mediaType, base64Data) {
            return (
                '<img style="max-height:100%; max-width:100%; border-radius: 14px;" src="data:' +
                mediaType +
                ';base64,' +
                base64Data +
                '" />'
            )
        }

        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }

        let parsedData = data
        if (typeof data === 'string') {
            parsedData = JSON.parse(data)
        }
        const mediaType = parsedData.mediaType || 'image/png'
        const base64 = parsedData.base64

        const width = this.dom.getAttributeNS(null, 'width')
        const height = this.dom.getAttributeNS(null, 'height')
        const tabElem = document.createElement('div')
        tabElem.setAttributeNS(null, 'id', 'vis-image-view')
        tabElem.setAttributeNS(null, 'viewBox', '0 0 ' + width + ' ' + height)
        tabElem.setAttributeNS(null, 'width', '100%')
        tabElem.setAttributeNS(null, 'height', '100%')

        tabElem.innerHTML = image(mediaType, base64)
        this.dom.appendChild(tabElem)
    }

    setSize(size) {
        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
    }
}

return ImageBase64Visualization
