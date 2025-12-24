unit ThreadSafeCollections.ErrorMessages;

{$mode objfpc}{$H+}

interface

const
  // Common error messages used across all thread-safe collections

  // Index and bounds errors
  ERR_INDEX_OUT_OF_BOUNDS = 'Index out of bounds';
  ERR_INVALID_INDEX_OR_COUNT = 'Invalid index or count';
  ERR_CAPACITY_LESS_THAN_COUNT = 'Capacity cannot be less than Count';
  ERR_START_INDEX_NEGATIVE = 'AStartIndex must be non-negative';

  // Collection state errors
  ERR_LIST_EMPTY = 'List is empty';
  ERR_DEQUE_EMPTY = 'Deque is empty';
  ERR_COLLECTION_EMPTY = 'Collection is empty';

  // Item errors
  ERR_ITEM_NOT_FOUND = 'Item not found';
  ERR_KEY_NOT_FOUND = 'Key not found';
  ERR_DUPLICATE_KEY = 'Duplicate key';

  // Argument errors
  ERR_COMPARER_REQUIRED = 'Comparer must be provided';
  ERR_ARRAY_TOO_SMALL = 'Destination array is too small';

  // Enumerator errors
  ERR_INVALID_ENUMERATOR_POSITION = 'Invalid enumerator position';

implementation

end.
