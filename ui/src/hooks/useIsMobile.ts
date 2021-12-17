import { Theme, useMediaQuery } from '@mui/material'

/**
 * Hook to check if client has "mobile" screen size. Useful for conditionally
 * rendering components in a responsive layout
 * @returns True if client has mobile screen size or screen size can't be determined
 */
export default function useIsMobile() {
  // Negate media query so that default SSR value for media query (false) shows
  // up as being mobile. In other words, if the media query fails, assume the
  // client is mobile and render for mobile first
  return !useMediaQuery<Theme>((theme) => theme.breakpoints.up('sm'))
}
