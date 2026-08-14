import { NextRequest, NextResponse } from "next/server";

export function middleware(request: NextRequest) {
  const host = request.headers.get("host") || "";
  // Also check x-forwarded-host, which is set by Phoenix's ReverseProxyPlug
  const forwardedHost = request.headers.get("x-forwarded-host") || "";
  const isShipnoiseHost =
    host.includes("shipnoise.net") || forwardedHost.includes("shipnoise.net");

  // Rewrite requests from shipnoise.net to /shipnoise pages
  if (isShipnoiseHost) {
    const url = request.nextUrl.clone();
    if (!url.pathname.startsWith("/shipnoise")) {
      url.pathname = `/shipnoise${url.pathname === "/" ? "" : url.pathname}`;
      return NextResponse.rewrite(url);
    }
  }

  return NextResponse.next();
}

export const config = {
  matcher: ["/((?!_next|api|static|favicon).*)"],
};
