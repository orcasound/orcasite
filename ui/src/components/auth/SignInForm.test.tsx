import { renderWithProviders, screen } from "@test/utils";

import SignInForm from "./SignInForm";

// SignInForm is presentational, so there's no network to stub here. It's the template for
// component tests generally
describe("SignInForm", () => {
  it("submits the credentials that were typed in", async () => {
    const onSubmit = vi.fn();
    const { user } = renderWithProviders(<SignInForm onSubmit={onSubmit} />);

    await user.type(screen.getByLabelText(/email/i), "orca@example.test");
    await user.type(screen.getByLabelText(/password/i), "hunter2");
    await user.click(screen.getByRole("button", { name: /sign in/i }));

    expect(onSubmit).toHaveBeenCalledTimes(1);
    expect(onSubmit).toHaveBeenCalledWith({
      email: "orca@example.test",
      password: "hunter2",
    });
  });

  it("does not submit anything else when the form is empty", async () => {
    const onSubmit = vi.fn();
    const { user } = renderWithProviders(<SignInForm onSubmit={onSubmit} />);

    await user.click(screen.getByRole("button", { name: /sign in/i }));

    expect(onSubmit).toHaveBeenCalledTimes(1);
    expect(onSubmit).toHaveBeenCalledWith({ email: "", password: "" });
  });

  it("turns invalid_credentials into a message a person can act on", () => {
    renderWithProviders(
      <SignInForm onSubmit={vi.fn()} errors={["invalid_credentials"]} />,
    );

    expect(screen.getByRole("alert")).toHaveTextContent(
      /email and password didn't match our records/i,
    );
  });

  it("falls back to showing an unrecognised error code", () => {
    renderWithProviders(
      <SignInForm onSubmit={vi.fn()} errors={["rate_limited"]} />,
    );

    expect(screen.getByRole("alert")).toHaveTextContent(
      /An error occurred: rate_limited/,
    );
  });

  it("shows no alert when there are no errors", () => {
    renderWithProviders(<SignInForm onSubmit={vi.fn()} />);

    expect(screen.queryByRole("alert")).not.toBeInTheDocument();
  });
});
