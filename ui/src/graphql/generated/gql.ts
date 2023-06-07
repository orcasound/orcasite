/* eslint-disable */
import * as types from './graphql'
import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core'

/**
 * Map of all GraphQL operations in the project.
 *
 * This map has several performance disadvantages:
 * 1. It is not tree-shakeable, so it will include all operations in the project.
 * 2. It is not minifiable, so the string of a GraphQL query will be multiple times inside the bundle.
 * 3. It does not support dead code elimination, so it will add unused operations.
 *
 * Therefore it is highly recommended to use the babel or swc plugin for production.
 */
const documents = {
  '\n  query feeds {\n    feeds {\n      id\n      name\n      slug\n      nodeName\n      locationPoint\n      thumbUrl\n      mapUrl\n    }\n  }\n':
    types.FeedsDocument,
  '\n  query feed($slug: String!) {\n    feed(slug: $slug) {\n      id\n      name\n      slug\n      nodeName\n      locationPoint\n      introHtml\n      thumbUrl\n      mapUrl\n    }\n  }\n':
    types.FeedDocument,
}

/**
 * The graphql function is used to parse GraphQL queries into a document that can be used by GraphQL clients.
 *
 *
 * @example
 * ```ts
 * const query = graphql(`query GetUser($id: ID!) { user(id: $id) { name } }`);
 * ```
 *
 * The query argument is unknown!
 * Please regenerate the types.
 */
export function graphql(source: string): unknown

/**
 * The graphql function is used to parse GraphQL queries into a document that can be used by GraphQL clients.
 */
export function graphql(
  source: '\n  query feeds {\n    feeds {\n      id\n      name\n      slug\n      nodeName\n      locationPoint\n      thumbUrl\n      mapUrl\n    }\n  }\n'
): (typeof documents)['\n  query feeds {\n    feeds {\n      id\n      name\n      slug\n      nodeName\n      locationPoint\n      thumbUrl\n      mapUrl\n    }\n  }\n']
/**
 * The graphql function is used to parse GraphQL queries into a document that can be used by GraphQL clients.
 */
export function graphql(
  source: '\n  query feed($slug: String!) {\n    feed(slug: $slug) {\n      id\n      name\n      slug\n      nodeName\n      locationPoint\n      introHtml\n      thumbUrl\n      mapUrl\n    }\n  }\n'
): (typeof documents)['\n  query feed($slug: String!) {\n    feed(slug: $slug) {\n      id\n      name\n      slug\n      nodeName\n      locationPoint\n      introHtml\n      thumbUrl\n      mapUrl\n    }\n  }\n']

export function graphql(source: string) {
  return (documents as any)[source] ?? {}
}

export type DocumentType<TDocumentNode extends DocumentNode<any, any>> =
  TDocumentNode extends DocumentNode<infer TType, any> ? TType : never
