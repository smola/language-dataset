namespace ZExcel\Worksheet;

class ColumnIterator implements \Iterator
{
    /**
     * \ZExcel\Worksheet to iterate
     *
     * @var \ZExcel\Worksheet
     */
    private subject;

    /**
     * Current iterator position
     *
     * @var int
     */
    private position = 0;

    /**
     * Start position
     *
     * @var int
     */
    private startColumn = 0;


    /**
     * End position
     *
     * @var int
     */
    private endColumn = 0;


    /**
     * Create a new column iterator
     *
     * @param    \ZExcel\Worksheet    $subject    The worksheet to iterate over
     * @param    string                $startColumn    The column address at which to start iterating
     * @param    string                $endColumn        Optionally, the column address at which to stop iterating
     */
    public function __construct(<\ZExcel\Worksheet> subject = null, string startColumn = "A", string endColumn = null)
    {
        // Set subject
        let this->subject = subject;
        
        this->resetEnd(endColumn);
        this->resetStart(startColumn);
    }

    /**
     * Destructor
     */
    public function __destruct()
    {
        unset(this->subject);
    }

    /**
     * (Re)Set the start column and the current column pointer
     *
     * @param integer    $startColumn    The column address at which to start iterating
     * @return \ZExcel\Worksheet\ColumnIterator
     */
    public function resetStart(string startColumn = "A") -> <\ZExcel\Worksheet\ColumnIterator>
    {
        var startColumnIndex;
        
        let startColumnIndex = \ZExcel\Cell::columnIndexFromString(startColumn) - 1;
        let this->startColumn = startColumnIndex;
        
        this->seek(startColumn);

        return this;
    }

    /**
     * (Re)Set the end column
     *
     * @param string    $endColumn    The column address at which to stop iterating
     * @return \ZExcel\Worksheet\ColumnIterator
     */
    public function resetEnd(var endColumn = null) -> <\ZExcel\Worksheet\ColumnIterator>
    {
        let endColumn = (endColumn !== null) ? endColumn : this->subject->getHighestColumn();
        let this->endColumn = \ZExcel\Cell::columnIndexFromString(endColumn) - 1;

        return this;
    }

    /**
     * Set the column pointer to the selected column
     *
     * @param string    $column    The column address to set the current pointer at
     * @return \ZExcel\Worksheet\ColumnIterator
     * @throws \ZExcel\Exception
     */
    public function seek(column = "A") -> <\ZExcel\Worksheet\ColumnIterator>
    {
        let column = \ZExcel\Cell::columnIndexFromString(column) - 1;
        
        if ((column < this->startColumn) || (column > this->endColumn)) {
            throw new \ZExcel\Exception("Column " . column . " is out of range (" . this->startColumn . " - " . this->endColumn . ")");
        }
        
        let this->position = column;

        return this;
    }

    /**
     * Rewind the iterator to the starting column
     */
    public function rewind()
    {
        let this->position = this->startColumn;
    }

    /**
     * Return the current column in this worksheet
     *
     * @return \ZExcel\Worksheet\Column
     */
    public function current() -> <\ZExcel\Worksheet\Column>
    {
        return new \ZExcel\Worksheet\Column(this->subject, \ZExcel\Cell::stringFromColumnIndex(this->position));
    }

    /**
     * Return the current iterator key
     *
     * @return string
     */
    public function key()
    {
        return \ZExcel\Cell::stringFromColumnIndex(this->position);
    }

    /**
     * Set the iterator to its next value
     */
    public function next()
    {
        let this->position = this->position + 1;
    }

    /**
     * Set the iterator to its previous value
     *
     * @throws \ZExcel\Exception
     */
    public function prev()
    {
        if (this->position <= this->startColumn) {
            throw new \ZExcel\Exception(
                "Column is already at the beginning of range (" .
                \ZExcel\Cell::stringFromColumnIndex(this->endColumn) . " - " .
                \ZExcel\Cell::stringFromColumnIndex(this->endColumn) . ")"
            );
        }

        let this->position = this->position - 1;
    }

    /**
     * Indicate if more columns exist in the worksheet range of columns that we're iterating
     *
     * @return boolean
     */
    public function valid() -> boolean
    {
        return this->position <= this->endColumn;
    }
}
