
// Package tracking state
let packageTracker = {
    total: 0,
    installed: [],
    pending: [],
    allInstalled: false
};

// Functions to manage package tracking
export function initializePackageTracker(packages) {
    packageTracker = {
        total: packages.length,
        installed: [],
        pending: packages,
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