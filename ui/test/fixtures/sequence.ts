let next = 1;

/** An id no other fixture in the run has, like the API's prefixed ids. */
export const uniqueId = (prefix: string) => `${prefix}_${next++}`;
