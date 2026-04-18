
// Package tracking state
let packageTracker = {
    total: 0,
    installed: [],
    pending: [],
    deps: {},
    allInstalled: false
};

// Functions to manage package tracking
export function initializePackageTracker(packages, depsEntries) {
    const deps = {};
    if (depsEntries) {
        for (const entry of depsEntries) {
            deps[entry[0]] = entry[1] ? [...entry[1]] : [];
        }
    }
    packageTracker = {
        total: packages.length,
        installed: [],
        pending: [...packages],
        deps,
        allInstalled: packages.length === 0
    };
    return packageTracker.allInstalled;
}

export function updatePackageTracker(packageName) {
    // Find the package in pending list
    const index = packageTracker.pending.indexOf(packageName);

    if (index !== -1) {
        // Remove from pending
        packageTracker.pending.splice(index, 1);
        // Add to installed
        packageTracker.installed.push(packageName);

        // Update allInstalled flag
        packageTracker.allInstalled = packageTracker.pending.length === 0;
    }

    return { ...packageTracker };
}

export function getPackageTrackerStatus() {
    return { ...packageTracker };
}

// Return the set of packages that never reported `package_installed`
// plus any packages that transitively depend on them (via the `:deps`
// relationship captured at initialization).
export function getFailedPackages() {
    const failed = new Set(packageTracker.pending);
    const reverseDeps = {};
    for (const [pkg, deps] of Object.entries(packageTracker.deps)) {
        for (const d of deps) {
            if (!reverseDeps[d]) reverseDeps[d] = [];
            reverseDeps[d].push(pkg);
        }
    }
    const queue = [...failed];
    while (queue.length) {
        const current = queue.shift();
        const dependents = reverseDeps[current] || [];
        for (const dep of dependents) {
            if (!failed.has(dep)) {
                failed.add(dep);
                queue.push(dep);
            }
        }
    }
    return [...failed];
}
