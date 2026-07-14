/**
 * plumber2-base post-login Action (deployed by deploy/auth0/provision.R).
 *
 * Exposes the user's roles and email-verification status as namespaced custom
 * claims. back authorizes on the ACCESS-token claims (the shiny-base
 * Action decorates only the ID token, which the API never sees); the
 * ID-token roles claim powers front-end UI
 * only. Claims are namespaced under the front-end origin, so tokens minted for
 * other apps in the tenant (shiny-base) merely carry extra inert claims.
 */
exports.onExecutePostLogin = async (event, api) => {
    const namespace = "https://plumber2-base.ma-riviere.com/";
    const roles = (event.authorization && event.authorization.roles) || [];
    api.idToken.setCustomClaim(`${namespace}roles`, roles);
    api.accessToken.setCustomClaim(`${namespace}roles`, roles);
    api.accessToken.setCustomClaim(`${namespace}email_verified`, event.user.email_verified === true);
};
